make_quarterly_price_lines <- function(all_data, plot_dir, min_n = 5, stat = c("median", "mean")) {
  
  stat <- match.arg(stat)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 with install.packages('ggplot2')")
  }
  
  needed <- c("quarter", "prix_m2", "Type.local", "Commune")
  if (!all(needed %in% names(all_data))) {
    stop("Missing columns: ", paste(setdiff(needed, names(all_data)), collapse = ", "))
  }
  
  d <- subset(all_data,
              !is.na(quarter) &
                !is.na(prix_m2) &
                !is.na(Type.local) &
                !is.na(Commune))
  
  # counts per quarter/city/type
  n_tbl <- aggregate(prix_m2 ~ quarter + Commune + Type.local, data = d, FUN = length)
  names(n_tbl)[names(n_tbl) == "prix_m2"] <- "n"
  
  # central tendency per quarter/city/type
  FUN <- if (stat == "median") median else mean
  agg_tbl <- aggregate(prix_m2 ~ quarter + Commune + Type.local, data = d, FUN = FUN)
  
  # merge + filter out sparse groups (prevents n=1 spikes)
  agg_tbl <- merge(agg_tbl, n_tbl, by = c("quarter", "Commune", "Type.local"))
  agg_tbl <- agg_tbl[agg_tbl$n >= min_n, ]
  
  # plot
  p <- ggplot2::ggplot(
    agg_tbl,
    ggplot2::aes(x = quarter, y = prix_m2, color = Commune, linetype = Type.local)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = paste0("Quarterly ", stat, " €/m² by city and property type (n ≥ ", min_n, ")"),
      x = "Quarter",
      y = paste0(stat, " €/m²"),
      color = "City",
      linetype = "Property type"
    ) +
    ggplot2::theme_minimal()
  
  out_file <- file.path(plot_dir, "04_quarterly_price_lines.png")
  ggplot2::ggsave(out_file, p, width = 12, height = 6, dpi = 150)
  message("Saved plot: ", out_file)
  
  invisible(agg_tbl)
}