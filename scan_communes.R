scan_communes_zip <- function(data_dir = "data",
                              pattern = "^full_.*\\.rds$",
                              out_csv = file.path(data_dir, "commune_zip_lookup.csv"),
                              max_rows_per_file = Inf) {
  
  files <- list.files(data_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files matched: ", pattern, " in ", data_dir)
  
  to_num_fr <- function(x) suppressWarnings(as.integer(gsub(" ", "", x)))
  
  out <- list()
  k <- 0
  
  for (f in files) {
    message("Reading: ", basename(f))
    df <- readRDS(f)
    
    # standardize names
    names(df) <- make.names(names(df))
    
    needed <- c("Commune", "Code.postal")
    if (!all(needed %in% names(df))) {
      message("Skipping (missing columns): ", basename(f))
      next
    }
    
    # keep just 2 cols
    d <- df[, needed, drop = FALSE]
    
    # optional limit
    if (is.finite(max_rows_per_file) && nrow(d) > max_rows_per_file) {
      d <- d[seq_len(max_rows_per_file), , drop = FALSE]
    }
    
    # normalize
    d$Commune <- toupper(trimws(as.character(d$Commune)))
    d$Code.postal <- to_num_fr(d$Code.postal)
    
    # drop NA
    d <- d[!is.na(d$Commune) & !is.na(d$Code.postal), , drop = FALSE]
    if (nrow(d) == 0) next
    
    # unique pairs in this file
    d <- unique(d)
    
    k <- k + 1
    out[[k]] <- d
    
    rm(df); gc()
  }
  
  all_pairs <- unique(do.call(rbind, out))
  all_pairs <- all_pairs[order(all_pairs$Commune, all_pairs$Code.postal), ]
  
  utils::write.csv(all_pairs, out_csv, row.names = FALSE)
  message("Wrote lookup: ", out_csv, " (", nrow(all_pairs), " rows)")
  
  all_pairs
}

lookup <- scan_communes_zip(data_dir = "data", pattern = "^full_.*\\.rds$")
View(head(lookup, 1000))

##Search:
subset(lookup, Commune == "LA ROCHE SUR FORON")
