project_dir <- normalizePath(getwd())
out_dir <- file.path(project_dir, "data")

read_filter_dvf_chunked <- function(txt_path,
                                    keep_commune = "LA ROCHE SUR FORON",
                                    chunk_lines = 200000) {
  con <- file(txt_path, open = "r", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  
  header_line <- readLines(con, n = 1)
  if (length(header_line) == 0) stop("Empty file: ", txt_path)
  
  col_names <- strsplit(header_line, "\\|")[[1]]
  
  out <- NULL
  
  repeat {
    lines <- readLines(con, n = chunk_lines)
    if (length(lines) == 0) break
    
    chunk <- read.table(
      text = lines,
      header = FALSE,
      sep = "|",
      fill = TRUE,
      quote = "",
      comment.char = "",
      stringsAsFactors = FALSE
    )
    
    if (nrow(chunk) == 0) next
    
    # align columns to header (DVF can be ragged)
    if (ncol(chunk) < length(col_names)) {
      chunk[(ncol(chunk) + 1):length(col_names)] <- NA
    } else if (ncol(chunk) > length(col_names)) {
      chunk <- chunk[, seq_len(length(col_names)), drop = FALSE]
    }
    names(chunk) <- col_names
    
    if (!("Commune" %in% names(chunk))) stop("Column 'Commune' not found: ", txt_path)
    if (!("Code postal" %in% names(chunk))) stop("Column 'Code postal' not found: ", txt_path)
    
    cp <- suppressWarnings(as.numeric(chunk[["Code postal"]]))
    keep <- (!is.na(cp)) & (toupper(chunk$Commune) == keep_commune) #& (cp == keep_zip)
    
    if (any(keep)) {
      kept <- chunk[keep, , drop = FALSE]
      out <- if (is.null(out)) kept else rbind(out, kept)
    }
    
    rm(chunk)
    gc()
  }
  
  if (is.null(out)) {
    out <- as.data.frame(setNames(replicate(length(col_names), logical(0), simplify = FALSE), col_names))
  }
  
  out
}

# Read every extracted DVF txt in /data and save filtered outputs
txt_files <- list.files(out_dir, pattern = "\\.txt$", full.names = TRUE)

annecy_results <- list()

for (txt_path in txt_files) {
  message("Filtering: ", txt_path)
  
  df_r <- read_filter_dvf_chunked(
    txt_path,
    keep_commune = "LA ROCHE SUR FORON",
    chunk_lines = 200000
  )
  
  key <- tools::file_path_sans_ext(basename(txt_path))
  annecy_results[[key]] <- df_r
  
  saveRDS(df_r, file.path(out_dir, paste0("larochesurforon_", key, ".rds")))
  
  rm(df_r)
  gc()
}

keep <- c("project_dir", "larochesurforon_results")

# Example: annecy_results[["ValeursFoncieres-2024"]]