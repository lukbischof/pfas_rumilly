# analysis/13_transaction_volume_maison_poisson.R
# ============================================================
# Poisson DiD on house transaction counts with TWO event windows
# Events:
#   1) 2022-11-16
#   2) 2023-11-28
#
# Outcome:
#   number of house transactions per commune-quarter
#
# Model:
#   sales ~ treat:shock1 + treat:shock2 + commune FE + quarter FE
#   estimated with Poisson and commune-clustered SE
# ============================================================

run_transaction_volume_poisson_maison <- function(all_data,
                                                  treat_commune = "RUMILLY",
                                                  event1 = as.Date("2022-11-16"),
                                                  event2 = as.Date("2023-11-28"),
                                                  window_days = 365,
                                                  cluster_var = "Commune") {
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required.")
  }
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' is required.")
  }
  
  df <- all_data
  names(df) <- make.names(names(df))
  
  needed <- c("Date.mutation", "Commune", "Type.local", "quarter")
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  if (!inherits(df$Date.mutation, "Date")) {
    df$Date.mutation <- as.Date(as.character(df$Date.mutation), format = "%d/%m/%Y")
  }
  if (!inherits(df$quarter, "Date")) {
    df$quarter <- as.Date(as.character(df$quarter))
  }
  
  # ---- houses only ----
  df <- subset(df, Type.local == "Maison")
  
  # ---- treatment indicator ----
  df$treat <- as.integer(df$Commune == treat_commune)
  
  # ---- event windows ----
  df$shock1 <- as.integer(
    df$Date.mutation >= event1 &
      df$Date.mutation < event1 + window_days
  )
  
  df$shock2 <- as.integer(
    df$Date.mutation >= event2 &
      df$Date.mutation < event2 + window_days
  )
  
  # ---- aggregate to commune-quarter counts ----
  agg <- aggregate(
    x = list(sales = rep(1L, nrow(df))),
    by = list(
      Commune = df$Commune,
      quarter = df$quarter,
      treat = df$treat,
      shock1 = df$shock1,
      shock2 = df$shock2
    ),
    FUN = sum
  )
  
  cat("\nPoisson DiD on house transaction counts\n")
  cat("Treat commune: ", treat_commune, "\n", sep = "")
  cat("Event 1: ", as.character(event1), "\n", sep = "")
  cat("Event 2: ", as.character(event2), "\n", sep = "")
  cat("Window: ", window_days, " days\n", sep = "")
  cat("Rows in aggregated dataset: ", nrow(agg), "\n", sep = "")
  cat("Total house transactions counted: ", sum(agg$sales), "\n\n", sep = "")
  
  # ---- model ----
  model <- glm(
    sales ~ treat:shock1 + treat:shock2 + factor(quarter) + factor(Commune),
    family = poisson(link = "log"),
    data = agg
  )
  
  # ---- clustered SE by commune ----
  if (!(cluster_var %in% names(agg))) {
    stop("cluster_var not found: ", cluster_var)
  }
  
  vc <- sandwich::vcovCL(model, cluster = agg[[cluster_var]])
  res <- lmtest::coeftest(model, vcov. = vc)
  
  keep <- rownames(res) %in% c("treat:shock1", "treat:shock2")
  print(res[keep, , drop = FALSE])
  
  # ---- percentage interpretation ----
  pct1 <- if ("treat:shock1" %in% names(coef(model))) 100 * (exp(coef(model)[["treat:shock1"]]) - 1) else NA_real_
  pct2 <- if ("treat:shock2" %in% names(coef(model))) 100 * (exp(coef(model)[["treat:shock2"]]) - 1) else NA_real_
  
  cat("\nImplied effect on house transaction volume:\n")
  cat("  Shock 1: ", round(pct1, 1), "%\n", sep = "")
  cat("  Shock 2: ", round(pct2, 1), "%\n", sep = "")
  
  invisible(list(
    model = model,
    results = res,
    aggregated_data = agg,
    implied_percent = c(shock1 = pct1, shock2 = pct2)
  ))
}