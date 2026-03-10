# analysis/10_did_controls.R

run_did_controls <- function(all_data,
                             treat_commune = "RUMILLY",
                             policy1 = as.Date("2022-11-16"),
                             policy2 = as.Date("2023-11-28"),
                             cluster_var = "Commune",
                             use_log = TRUE) {
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required.")
  }
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' is required.")
  }
  
  df <- all_data
  names(df) <- make.names(names(df))
  
  needed <- c(
    "Date.mutation", "Commune", "Type.local", "prix_m2",
    "Surface.reelle.bati", "Surface.terrain", "Nombre.pieces.principales"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Missing columns: ", paste(miss, collapse = ", "))
  }
  
  if (!inherits(df$Date.mutation, "Date")) {
    df$Date.mutation <- as.Date(as.character(df$Date.mutation), format = "%d/%m/%Y")
  }
  
  if (!("quarter" %in% names(df)) || !inherits(df$quarter, "Date")) {
    m <- as.integer(format(df$Date.mutation, "%m"))
    q_start_month <- ((m - 1) %/% 3) * 3 + 1
    df$quarter <- as.Date(sprintf("%s-%02d-01", format(df$Date.mutation, "%Y"), q_start_month))
  }
  
  df$treat <- as.integer(df$Commune == treat_commune)
  df$mid12 <- as.integer(df$Date.mutation >= policy1 & df$Date.mutation < policy2)
  df$post2 <- as.integer(df$Date.mutation >= policy2)
  
  if (use_log) {
    df$y <- log(df$prix_m2)
  } else {
    df$y <- df$prix_m2
  }
  
  # keep observations with controls and basic DVF filters
  df <- subset(
    df,
    !is.na(y) &
      !is.na(Commune) &
      !is.na(Type.local) &
      Type.local %in% c("Appartement", "Maison") &
      !is.na(Surface.reelle.bati) & Surface.reelle.bati >= 10 &
      !is.na(Surface.terrain) &
      !is.na(Nombre.pieces.principales) &
      !is.na(Valeur.fonciere) & Valeur.fonciere > 50000 & Valeur.fonciere < 2000000 &
      prix_m2 > 500 & prix_m2 < 15000
  )
  
  if (nrow(df) == 0) stop("No rows left after filtering.")
  
  add_factor_if_multi <- function(terms, df, varname) {
    if (varname %in% names(df) && length(unique(df[[varname]])) >= 2) {
      c(terms, paste0("factor(", varname, ")"))
    } else {
      terms
    }
  }
  
  rhs <- c(
    "treat:mid12",
    "treat:post2",
    "Surface.reelle.bati",
    "Surface.terrain",
    "Nombre.pieces.principales"
  )
  
  rhs <- add_factor_if_multi(rhs, df, "Type.local")
  rhs <- add_factor_if_multi(rhs, df, "quarter")
  rhs <- add_factor_if_multi(rhs, df, "Commune")
  
  f <- as.formula(paste("y ~", paste(rhs, collapse = " + ")))
  model <- lm(f, data = df)
  
  if (!(cluster_var %in% names(df))) stop("cluster_var not found: ", cluster_var)
  
  if (length(unique(df[[cluster_var]])) < 2) {
    vc <- sandwich::vcovHC(model, type = "HC1")
    res <- lmtest::coeftest(model, vcov. = vc)
    note <- paste0("NOTE: only 1 unique ", cluster_var, "; used HC1 robust SE.")
  } else {
    vc <- sandwich::vcovCL(model, cluster = df[[cluster_var]])
    res <- lmtest::coeftest(model, vcov. = vc)
    note <- NULL
  }
  
  cat("\nDiD regression with property controls\n")
  cat("Treat commune: ", treat_commune, "\n", sep = "")
  cat("N = ", nrow(df), "\n", sep = "")
  if (!is.null(note)) cat(note, "\n")
  
  keep <- rownames(res) %in% c("treat:mid12", "treat:post2")
  print(res[keep, , drop = FALSE])
  
  invisible(list(model = model, results = res, data_used = df))
}