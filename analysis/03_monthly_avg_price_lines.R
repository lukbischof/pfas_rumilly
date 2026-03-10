make_monthly_price_lines <- function(all_data, plot_dir, stat = c("mean", "median")) {
  
  stat <- match.arg(stat)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 with install.packages('ggplot2')")
  }
  
  needed <- c("month", "prix_m2", "Type.local", "Commune")
  if (!all(needed %in% names(all_data))) {
    stop("Missing columns: ", paste(setdiff(needed, names(all_data)), collapse = ", "))
  }
  
  d <- subset(all_data,
              !is.na(month) &
                !is.na(prix_m2) &
                !is.na(Type.local) &
                !is.na(Commune))
  
  FUN <- if (stat == "median") median else mean
  
  agg_tbl <- aggregate(prix_m2 ~ month + Commune + Type.local, data = d, FUN = FUN)
  
  p <- ggplot2::ggplot(
    agg_tbl,
    ggplot2::aes(x = month, y = prix_m2, color = Commune, linetype = Type.local)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = paste0("Monthly ", stat, " €/m² by city and property type"),
      x = "Month",
      y = paste0(stat, " €/m²"),
      color = "City",
      linetype = "Property type"
    ) +
    ggplot2::theme_minimal()
  
  out_file <- file.path(plot_dir, paste0("03_monthly_", stat, "_price_lines.png"))
  ggplot2::ggsave(out_file, p, width = 12, height = 6, dpi = 150)
  
  message("Saved plot: ", out_file)
  
  invisible(agg_tbl)
}