make_scatter_prix_m2_month <- function(all_data, plot_dir) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 with install.packages('ggplot2')")
  }
  
  needed <- c("Date.mutation", "prix_m2", "Commune")
  if (!all(needed %in% names(all_data))) {
    stop("Missing columns: ", paste(setdiff(needed, names(all_data)), collapse = ", "))
  }
  
  d <- all_data[
    !is.na(all_data$Date.mutation) &
    !is.na(all_data$prix_m2) &
    !is.na(all_data$Commune),
  ]

  d$month <- as.Date(format(d$Date.mutation, "%Y-%m-01"))
  
  p <- ggplot2::ggplot(d,
                       ggplot2::aes(
                         x = month,
                         y = prix_m2,
                         color = Commune
                       )) +
    ggplot2::geom_point(alpha = 0.35, size = 0.8) +
    ggplot2::labs(
      title = "Square meter price by month",
      x = "Month",
      y = "€/m²",
      color = "City"
    ) +
    ggplot2::theme_minimal()
  
  out_file <- file.path(plot_dir, "01_scatter_prix_m2_by_month.png")
  
  ggplot2::ggsave(out_file, p, width = 12, height = 6, dpi = 150)
  
  message("Saved plot: ", out_file)
  
  invisible(out_file)
}