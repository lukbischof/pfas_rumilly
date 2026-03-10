make_quarterly_rumilly_vs_others <- function(all_data, plot_dir,
                                             min_n = 5,
                                             stat = c("median", "mean")) {
  
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
  
  # ---- create Rumilly vs others group ----
  d$city_group <- ifelse(d$Commune == "RUMILLY", "Rumilly", "Not Rumilly")
  
  # counts per quarter/group/type
  n_tbl <- aggregate(prix_m2 ~ quarter + city_group + Type.local, data = d, FUN = length)
  names(n_tbl)[names(n_tbl) == "prix_m2"] <- "n"
  
  # central tendency
  FUN <- if (stat == "median") median else mean
  agg_tbl <- aggregate(prix_m2 ~ quarter + city_group + Type.local, data = d, FUN = FUN)
  
  # merge + filter out sparse groups
  agg_tbl <- merge(agg_tbl, n_tbl, by = c("quarter", "city_group", "Type.local"))
  agg_tbl <- agg_tbl[agg_tbl$n >= min_n, ]
  
  # plot
  p <- ggplot2::ggplot(
    agg_tbl,
    ggplot2::aes(
      x = quarter,
      y = prix_m2,
      color = city_group,
      linetype = Type.local
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = paste0("Quarterly ", stat, " €/m²: Rumilly vs Other Cities (n ≥ ", min_n, ")"),
      x = "Quarter",
      y = paste0(stat, " €/m²"),
      color = "City group",
      linetype = "Property type"
    ) +
    ggplot2::theme_minimal()
  
  out_file <- file.path(plot_dir, paste0("06_quarterly_rumilly_vs_others_", stat, "_n", min_n, ".png"))
  ggplot2::ggsave(out_file, p, width = 12, height = 6, dpi = 150)
  
  message("Saved plot: ", out_file)
  
  invisible(agg_tbl)
}