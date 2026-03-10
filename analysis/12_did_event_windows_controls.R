# analysis/12_did_event_windows_controls.R
# ============================================================
# Event-window DiD with property controls
#
# Events:
#   event1: first news shock date
#   event2: second news shock date
#
# Key idea:
#   Effects are assumed to occur ONLY within a limited window
#   after each event (default: 365 days).
#
# Model:
#   y ~ treat:shock1 + treat:shock2 + controls + FE
#
# Controls:
#   Surface.reelle.bati + Surface.terrain + Nombre.pieces.principales
#
# FE:
#   Commune FE + Quarter FE (+ Type.local FE when pooled)
#
# SE:
#   Clustered by Commune (sandwich + lmtest)
# ============================================================

run_event_window_did_controls <- function(all_data,
                                          treat_commune = "RUMILLY",
                                          event1 = as.Date("2022-11-16"),
                                          event2 = as.Date("2023-11-28"),
                                          window_days = 365,
                                          use_log = TRUE,
                                          cluster_var = "Commune",
                                          run_by_type = TRUE) {
  
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required. Install with install.packages('sandwich').")
  }
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' is required. Install with install.packages('lmtest').")
  }
  
  if (!is.data.frame(all_data)) stop("all_data must be a data.frame")
  
  df <- all_data
  names(df) <- make.names(names(df))
  
  needed <- c(
    "Date.mutation", "Commune", "Type.local", "prix_m2",
    "Surface.reelle.bati", "Surface.terrain",
    "Nombre.pieces.principales", "Valeur.fonciere"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss) > 0) {
    stop("Missing columns in all_data: ", paste(miss, collapse = ", "))
  }
  
  # Ensure Date
  if (!inherits(df$Date.mutation, "Date")) {
    df$Date.mutation <- as.Date(as.character(df$Date.mutation), format = "%d/%m/%Y")
  }
  
  # Quarter FE variable
  if (!("quarter" %in% names(df)) || !inherits(df$quarter, "Date")) {
    m <- as.integer(format(df$Date.mutation, "%m"))
    q_start_month <- ((m - 1) %/% 3) * 3 + 1
    df$quarter <- as.Date(sprintf("%s-%02d-01", format(df$Date.mutation, "%Y"), q_start_month))
  }
  
  # Treatment indicator
  df$treat <- as.integer(df$Commune == treat_commune)
  
  # Event-window indicators
  df$shock1 <- as.integer(df$Date.mutation >= event1 &
                            df$Date.mutation < (event1 + window_days))
  df$shock2 <- as.integer(df$Date.mutation >= event2 &
                            df$Date.mutation < (event2 + window_days))
  
  # Outcome
  if (use_log) {
    df$y <- log(df$prix_m2)
    y_label <- "log(prix_m2)"
  } else {
    df$y <- df$prix_m2
    y_label <- "prix_m2"
  }
  
  # Keep observations with controls + DVF hygiene filters
  df <- subset(
    df,
    !is.na(Date.mutation) &
      !is.na(y) &
      !is.na(Commune) &
      !is.na(Type.local) &
      Type.local %in% c("Appartement", "Maison") &
      !is.na(prix_m2) & prix_m2 > 500 & prix_m2 < 15000 &
      !is.na(Surface.reelle.bati) & Surface.reelle.bati >= 10 &
      !is.na(Surface.terrain) &
      !is.na(Nombre.pieces.principales) &
      !is.na(Valeur.fonciere) & Valeur.fonciere > 50000 & Valeur.fonciere < 2000000
  )
  
  if (nrow(df) == 0) stop("No rows left after filtering. Relax filters or check columns.")
  
  fit_one <- function(df_sub, label) {
    
    add_factor_if_multi <- function(terms, df, varname) {
      if (varname %in% names(df) && length(unique(df[[varname]])) >= 2) {
        c(terms, paste0("factor(", varname, ")"))
      } else {
        terms
      }
    }
    
    rhs <- c(
      "treat:shock1",
      "treat:shock2",
      "Surface.reelle.bati",
      "Surface.terrain",
      "Nombre.pieces.principales"
    )
    
    rhs <- add_factor_if_multi(rhs, df_sub, "Type.local")  # drops in type-specific runs
    rhs <- add_factor_if_multi(rhs, df_sub, "quarter")
    rhs <- add_factor_if_multi(rhs, df_sub, "Commune")
    
    f <- as.formula(paste("y ~", paste(rhs, collapse = " + ")))
    m <- lm(f, data = df_sub)
    
    if (!(cluster_var %in% names(df_sub))) stop("cluster_var not found: ", cluster_var)
    
    # Clustered SE; fallback to HC1 if only one cluster
    if (length(unique(df_sub[[cluster_var]])) < 2) {
      vc <- sandwich::vcovHC(m, type = "HC1")
      ct <- lmtest::coeftest(m, vcov. = vc)
      note <- paste0("NOTE: Only 1 unique ", cluster_var, "; used HC1 robust SE (not clustered).")
    } else {
      vc <- sandwich::vcovCL(m, cluster = df_sub[[cluster_var]])
      ct <- lmtest::coeftest(m, vcov. = vc)
      note <- NULL
    }
    
    list(
      label = label,
      model = m,
      coeftest = ct,
      n = nrow(df_sub),
      note = note
    )
  }
  
  out <- list(
    meta = list(
      outcome = y_label,
      treat_commune = treat_commune,
      event1 = event1,
      event2 = event2,
      window_days = window_days,
      cluster_var = cluster_var,
      controls = c("Surface.reelle.bati", "Surface.terrain", "Nombre.pieces.principales")
    )
  )
  
  out$pooled <- fit_one(df, "Pooled (Appartement + Maison)")
  
  if (run_by_type) {
    out$appartement <- fit_one(subset(df, Type.local == "Appartement"), "Appartement only")
    out$maison      <- fit_one(subset(df, Type.local == "Maison"), "Maison only")
  }
  
  # Compact print
  print_block <- function(res) {
    cat("\n===============================\n")
    cat(res$label, "\n")
    cat("N =", res$n, "\n")
    cat("===============================\n")
    if (!is.null(res$note)) cat(res$note, "\n")
    
    ct <- res$coeftest
    keep <- rownames(ct) %in% c("treat:shock1", "treat:shock2")
    print(ct[keep, , drop = FALSE])
  }
  
  cat("\nEvent-window DiD with property controls using ", out$meta$outcome, "\n", sep = "")
  cat("Treat commune: ", treat_commune, "\n", sep = "")
  cat("Event 1: ", as.character(event1), "  |  Event 2: ", as.character(event2), "\n", sep = "")
  cat("Window: ", window_days, " days\n", sep = "")
  cat("Controls: Surface.reelle.bati + Surface.terrain + Nombre.pieces.principales\n")
  cat("FE: quarter + commune (+ type in pooled) | Clustered SE by: ", cluster_var, "\n", sep = "")
  
  print_block(out$pooled)
  if (run_by_type) {
    print_block(out$appartement)
    print_block(out$maison)
  }
  
  invisible(out)
}