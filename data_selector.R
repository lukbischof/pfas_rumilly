# ============================================================
# data_selector.R — Load + cache + multi-select DVF subsets
# ============================================================
#
# What you (the user) must do to set paths:
#
# Run R with your WORKING DIRECTORY set to the AEER project folder
# (the folder that contains main.R, data_selector.R, the loader scripts,
#  and the data/ folder).
#
# Examples:
# - RStudio: open the AEER folder as a Project, OR
#   Session > Set Working Directory > Choose Directory... (pick AEER)
# - Base R: setwd("path/to/AEER")
#
# This file is meant to be SOURCED from main.R:
#   source("data_selector.R")
#
# It defines ONE main function you call:
#   out <- load_selected_data()
#
# out$loaded_data : nested list (dataset -> file -> data.frame)
# out$all_data    : one stacked data.frame containing everything loaded
# ============================================================

# ---- project paths ----
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# sanity check: this script should live in the project directory
if (!file.exists(file.path(project_dir, "data_selector.R"))) {
  stop(
    "You are NOT running from the AEER project folder.\n\n",
    "Current working directory:\n  ", project_dir, "\n\n",
    "Fix:\n",
    "  setwd(\"/path/to/AEER\")\n",
    "  source(\"data_selector.R\")\n\n",
    "The AEER folder must contain: data_selector.R and your load_*.R scripts."
  )
}

data_dir <- file.path(project_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# ---- registry of datasets ----
# Add/remove cities here. Make sure:
# - cache_glob matches filenames written by the loader (e.g., annecy_*.rds)
# - loader_script points to the correct loader file in the AEER folder
DATASETS <- list(
  rumilly = list(
    id = "rumilly",
    label = "Rumilly",
    cache_glob = "rumilly_*.rds",
    loader_script = file.path(project_dir, "load_dvf_rumilly.R")
  ),
  annecy = list(
    id = "annecy",
    label = "Annecy",
    cache_glob = "annecy_*.rds",
    loader_script = file.path(project_dir, "load_dvf_annecy.R")
  ),
  valserhone = list(
    id = "valserhone",
    label = "Valserhone",
    cache_glob = "valserhone_*.rds",
    loader_script = file.path(project_dir, "load_dvf_valserhone.R")
  ),
  belley = list(
    id = "belley",
    label = "Belley",
    cache_glob = "belley_*.rds",
    loader_script = file.path(project_dir, "load_dvf_belley.R")
  ),
  larochesurforon = list(
    id = "larochesurforon",
    label = "La Roche Sur Foron",
    cache_glob = "larochesurforon_*.rds",
    loader_script = file.path(project_dir, "load_dvf_larochesurforon.R")
  ),
  full = list(
    id = "full",
    label = "Full dataset (WARNING: this might cause your computer to crash)",
    cache_glob = "full_*.rds",  
    loader_script = file.path(project_dir, "load_dvf_full.R")
  )
)

# ---- helpers ----

# Convert a glob like "rumilly_*.rds" into a regex for list.files()
glob_to_regex <- function(glob) {
  rx <- glob
  rx <- gsub("\\.", "\\\\.", rx)  # escape dots
  rx <- gsub("\\*", ".*", rx)     # glob star -> regex .*
  paste0("^", rx, "$")
}

cache_files_for <- function(ds, data_dir) {
  rx <- glob_to_regex(ds$cache_glob)
  list.files(data_dir, pattern = rx, full.names = TRUE)
}

ensure_dataset_cached <- function(ds, project_dir, data_dir) {
  files <- cache_files_for(ds, data_dir)
  if (length(files) > 0) {
    message("[OK] Cache found for: ", ds$label, " (", length(files), " file(s))")
    return(invisible(TRUE))
  }
  
  message("[MISS] Cache not found for: ", ds$label)
  message("       Running loader: ", ds$loader_script)
  
  if (!file.exists(ds$loader_script)) {
    stop(
      "Loader script not found: ", ds$loader_script, "\n",
      "Fix: ensure the loader file exists next to data_selector.R in the AEER folder."
    )
  }
  
  # Run loader in its own environment; provide project_dir/data_dir
  loader_env <- new.env(parent = globalenv())
  loader_env$project_dir <- project_dir
  loader_env$data_dir <- data_dir
  
  source(ds$loader_script, local = loader_env)
  
  # Re-check after loader
  files2 <- cache_files_for(ds, data_dir)
  if (length(files2) == 0) {
    stop(
      "Loader ran but no cache files were created for: ", ds$label, "\n",
      "Expected pattern: ", ds$cache_glob, "\n",
      "Looked in: ", data_dir, "\n",
      "Fix: ensure the loader saves .rds files with this prefix/pattern into /data."
    )
  }
  
  message("[OK] Cache created for: ", ds$label, " (", length(files2), " file(s))")
  invisible(TRUE)
}

load_dataset_from_cache <- function(ds, data_dir) {
  files <- cache_files_for(ds, data_dir)
  if (length(files) == 0) stop("No cache files to load for: ", ds$label)
  
  objs <- lapply(files, readRDS)
  names(objs) <- tools::file_path_sans_ext(basename(files))
  objs
}

# ---- MULTISELECT (console; works everywhere) ----
select_datasets <- function(DATASETS) {
  choices <- vapply(DATASETS, `[[`, character(1), "label")
  ids <- vapply(DATASETS, `[[`, character(1), "id")
  
  if (!interactive()) {
    message("Non-interactive session: defaulting to ALL datasets.")
    return(ids)
  }
  
  repeat {
    cat("\nSelect one or more datasets by number (comma/space separated).\n")
    for (i in seq_along(choices)) cat(sprintf("  %d) %s\n", i, choices[i]))
    cat("Example: 1,3\n")
    cat("Type 'q' to quit.\n")
    
    ans <- trimws(readline("Your selection: "))
    if (tolower(ans) %in% c("q", "quit", "exit")) stop("Cancelled by user.")
    
    parts <- unlist(strsplit(ans, "[,\\s]+"))
    parts <- parts[nzchar(parts)]
    idx <- suppressWarnings(as.integer(parts))
    idx <- idx[!is.na(idx)]
    
    if (length(idx) < 1) {
      cat("Please select at least 1 dataset.\n")
      next
    }
    if (any(idx < 1 | idx > length(ids))) {
      cat("Invalid selection: use numbers between 1 and ", length(ids), ".\n", sep = "")
      next
    }
    
    idx <- unique(idx)
    return(ids[idx])
  }
}

# Robust stack that tolerates differing columns (fills missing with NA)
safe_rbind_fill <- function(dfs) {
  if (length(dfs) == 0) return(data.frame())
  all_cols <- unique(unlist(lapply(dfs, names)))
  dfs2 <- lapply(dfs, function(df) {
    miss <- setdiff(all_cols, names(df))
    if (length(miss)) df[miss] <- NA
    df[all_cols]
  })
  do.call(rbind, dfs2)
}

# ---- internal runner (loads nested list) ----
run_selector <- function() {
  selected_ids <- select_datasets(DATASETS)
  
  selected <- DATASETS[names(DATASETS) %in% selected_ids]
  if (length(selected) < 1) stop("No valid datasets selected.")
  
  invisible(lapply(selected, ensure_dataset_cached,
                   project_dir = project_dir, data_dir = data_dir))
  
  loaded_data <- lapply(selected, load_dataset_from_cache, data_dir = data_dir)
  names(loaded_data) <- vapply(selected, `[[`, character(1), "id")
  
  loaded_data
}

# ============================================================
# PUBLIC FUNCTION
# Call this from main.R:
#   out <- load_selected_data()
#   out$loaded_data  (nested list)
#   out$all_data     (stacked data.frame)
# ============================================================
load_selected_data <- function() {
  message("Project directory: ", project_dir)
  message("Data directory:    ", data_dir)
  
  loaded_data <- run_selector()
  
  # ============================================================
  # BASIC DATA CLEANING (applied to each loaded dataset)
  # ============================================================
  
  clean_one <- function(df) {
    
    # ---- standardize column names ----
    names(df) <- make.names(names(df))
    
    # ---- convert DVF date ----
    if ("Date.mutation" %in% names(df)) {
      df$Date.mutation <- as.Date(as.character(df$Date.mutation), format = "%d/%m/%Y")
    }
    
    # ---- convert numeric columns ----
    to_num_fr <- function(x) as.numeric(gsub(",", ".", gsub(" ", "", x)))
    
    num_cols <- c(
      "Valeur.fonciere",
      "Surface.reelle.bati",
      "Surface.terrain",
      "Nombre.pieces.principales"
    )
    
    for (col in num_cols) {
      if (col %in% names(df)) {
        df[[col]] <- to_num_fr(df[[col]])
      }
    }
    
    # ---- filter subset of interest ----
    needed <- c("Nature.mutation", "Valeur.fonciere", "Surface.reelle.bati", "Type.local")
    
    if (!all(needed %in% names(df))) return(NULL)
    
    df <- df[
      df$Nature.mutation == "Vente" &
        df$Valeur.fonciere > 0 &
        df$Surface.reelle.bati > 0 &
        df$Type.local %in% c("Maison", "Appartement"),
    ]
    
    df
  }
  
  # apply cleaning to every dataset
  loaded_data <- lapply(loaded_data, function(city_list) {
    lapply(city_list, clean_one)
  })
  
  # ============================================================
  # QUICK SANITY CHECK
  # ============================================================
  
  flat_check <- unlist(loaded_data, recursive = FALSE)
  
  message("Cleaning check:")
  message("  datasets after cleaning: ", length(flat_check))
  
  if (length(flat_check) > 0) {
    message("  rows in first dataset: ", nrow(flat_check[[1]]))
    message("  date class: ", class(flat_check[[1]]$Date.mutation))
    message("  Valeur.fonciere class: ", class(flat_check[[1]]$Valeur.fonciere))
  }
  
  message("\nLoaded datasets: ", paste(names(loaded_data), collapse = ", "))
  
  flat <- unlist(loaded_data, recursive = FALSE)
  message("Files loaded total: ", length(flat))
  
  all_data <- safe_rbind_fill(flat)
  message("Combined rows: ", nrow(all_data), " | columns: ", ncol(all_data))
  
  # Return both forms so you can use whichever is convenient
  list(
    loaded_data = loaded_data,
    all_data = all_data
  )
}