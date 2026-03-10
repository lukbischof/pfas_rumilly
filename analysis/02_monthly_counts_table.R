make_monthly_counts_table <- function(all_data, table_dir) {
  
  needed <- c("Date.mutation", "Type.local", "Commune")
  if (!all(needed %in% names(all_data))) {
    stop("Missing columns: ", paste(setdiff(needed, names(all_data)), collapse = ", "))
  }
  
  d <- all_data[!is.na(all_data$Date.mutation), ]
  
  d$month <- as.Date(format(d$Date.mutation, "%Y-%m-01"))
  
  # ---- total transactions ----
  tot <- aggregate(Type.local ~ month + Commune, data = d, FUN = length)
  names(tot)[names(tot) == "Type.local"] <- "n_total"
  
  # ---- apartments ----
  apt <- d[d$Type.local == "Appartement", ]
  apt <- aggregate(Type.local ~ month + Commune, data = apt, FUN = length)
  names(apt)[names(apt) == "Type.local"] <- "n_appartement"
  
  # ---- houses ----
  hou <- d[d$Type.local == "Maison", ]
  hou <- aggregate(Type.local ~ month + Commune, data = hou, FUN = length)
  names(hou)[names(hou) == "Type.local"] <- "n_maison"
  
  # ---- merge tables ----
  out <- merge(tot, apt, by = c("month", "Commune"), all.x = TRUE)
  out <- merge(out, hou, by = c("month", "Commune"), all.x = TRUE)
  
  out$n_appartement[is.na(out$n_appartement)] <- 0
  out$n_maison[is.na(out$n_maison)] <- 0
  
  out <- out[order(out$month, out$Commune), ]
  
  out_file <- file.path(table_dir, "02_monthly_counts_by_city.csv")
  
  write.csv(out, out_file, row.names = FALSE)
  
  message("Saved table: ", out_file)
  
  return(out)
}