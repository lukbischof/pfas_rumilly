plot_transaction_composition <- function(all_data,
                                         plot_dir,
                                         table_dir = NULL,
                                         event_date = as.Date("2023-11-28"),
                                         treat_commune = "RUMILLY") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install ggplot2 with install.packages('ggplot2')")
  }
  
  df <- all_data
  names(df) <- make.names(names(df))
  
  # Maison only
  df <- subset(df, Type.local == "Maison")
  
  # Period and group
  df$period <- ifelse(df$Date.mutation < event_date, "Before event", "After event")
  df$group  <- ifelse(df$Commune == treat_commune, "Rumilly", "Control communes")
  
  # Keep needed vars
  df <- subset(
    df,
    !is.na(Surface.reelle.bati) &
      !is.na(Surface.terrain) &
      !is.na(Nombre.pieces.principales)
  )
  
  if (nrow(df) == 0) stop("No Maison observations left for composition plot.")
  
  # Means
  avg <- aggregate(
    cbind(Surface.reelle.bati,
          Surface.terrain,
          Nombre.pieces.principales) ~ group + period,
    data = df,
    mean
  )
  
  # Counts too
  n_tab <- aggregate(
    Surface.reelle.bati ~ group + period,
    data = df,
    FUN = length
  )
  names(n_tab)[names(n_tab) == "Surface.reelle.bati"] <- "n"
  
  avg <- merge(avg, n_tab, by = c("group", "period"))
  
  # Save summary table if desired
  if (!is.null(table_dir)) {
    dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
    out_csv <- file.path(table_dir, "11_transaction_composition_maison_summary.csv")
    utils::write.csv(avg, out_csv, row.names = FALSE)
    message("Saved table: ", out_csv)
  }
  
  # Long format for plotting
  plot_data <- rbind(
    data.frame(group = avg$group, period = avg$period, n = avg$n,
               variable = "Built surface (m²)", value = avg$Surface.reelle.bati),
    data.frame(group = avg$group, period = avg$period, n = avg$n,
               variable = "Land size (m²)", value = avg$Surface.terrain),
    data.frame(group = avg$group, period = avg$period, n = avg$n,
               variable = "Number of rooms", value = avg$Nombre.pieces.principales)
  )
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = period, y = value, fill = period)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_grid(variable ~ group, scales = "free_y") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Composition of house transactions before and after contamination news",
      x = "",
      y = "Average value"
    ) +
    ggplot2::theme(legend.position = "none")
  
  out_png <- file.path(plot_dir, "11_transaction_composition_maison.png")
  ggplot2::ggsave(out_png, plot = p, width = 9, height = 7, dpi = 300)
  
  message("Saved plot: ", out_png)
  print(avg)
  
  invisible(avg)
}