out_dir <- project_dir <- normalizePath(getwd())
out_dir <- file.path(project_dir, "data")file.path(project_dir, "data")

read_dvf_chunked_all <- function(txt_path, chunk_lines = 200000) {
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
    
    out <- if (is.null(out)) chunk else rbind(out, chunk)
    
    rm(chunk)
    gc()
  }
  
  if (is.null(out)) {
    out <- as.data.frame(setNames(replicate(length(col_names), logical(0), simplify = FALSE), col_names))
  }
  
  out
}

# Read every extracted DVF txt in /data and save outputs
txt_files <- list.files(out_dir, pattern = "\\.txt$", full.names = TRUE)

dvf_results <- list()

for (txt_path in txt_files) {
  message("Reading ALL rows: ", txt_path)
  
  df_all <- read_dvf_chunked_all(txt_path, chunk_lines = 200000)
  
  key <- tools::file_path_sans_ext(basename(txt_path))
  dvf_results[[key]] <- df_all
  
  saveRDS(df_all, file.path(out_dir, paste0("full_", key, ".rds")))
  
  rm(df_all)
  gc()
}

keep <- c("project_dir", "dvf_results")

# Example: dvf_results[["ValeursFoncieres-2024"]]
