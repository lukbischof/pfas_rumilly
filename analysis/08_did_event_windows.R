# analysis/08_did_event_windows.R
# ============================================================
# Temporary event-window DiD for Rumilly "news shock" events
#
# Events (news dates):
#   event1: 2022-11-16
#   event2: 2023-11-28
#
# Key idea:
#   Effects are assumed to occur ONLY within a limited window after each event.
#   We define window indicators (shock1, shock2) and estimate:
#     y ~ treat:shock1 + treat:shock2 + FE
#
# FE:
#   Commune FE + Quarter FE (+ Type.local FE when pooled)
#
# SE:
#   Clustered by Commune (sandwich + lmtest)
# ============================================================

run_event_window_did <- function(all_data,
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
  
  names(all_data) <- make.names(names(all_data))
  
  needed <- c("Date.mutation", "Commune", "Type.local", "prix_m2",
              "Surface.reelle.bati", "Valeur.fonciere")
  miss <- setdiff(needed, names(all_data))
  if (length(miss) > 0) stop("Missing columns in all_data: ", paste(miss, collapse = ", "))
  
  # Ensure Date
  if (!inherits(all_data$Date.mutation, "Date")) {
    all_data$Date.mutation <- as.Date(as.character(all_data$Date.mutation), format = "%d/%m/%Y")
  }
  
  # Quarter FE var
  if (!("quarter" %in% names(all_data)) || !inherits(all_data$quarter, "Date")) {
    m <- as.integer(format(all_data$Date.mutation, "%m"))
    q_start_month <- ((m - 1) %/% 3) * 3 + 1
    all_data$quarter <- as.Date(sprintf("%s-%02d-01", format(all_data$Date.mutation, "%Y"), q_start_month))
  }
  
  # DVF hygiene filters (same as 07_)
  all_data <- subset(
    all_data,
    !is.na(Date.mutation) &
      !is.na(Commune) &
      !is.na(Type.local) &
      Type.local %in% c("Appartement", "Maison") &
      !is.na(prix_m2) & prix_m2 > 0 &
      !is.na(Surface.reelle.bati) & Surface.reelle.bati >= 10 &
      !is.na(Valeur.fonciere) & Valeur.fonciere > 50000 & Valeur.fonciere < 2000000 &
      prix_m2 > 500 & prix_m2 < 15000
  )
  if (nrow(all_data) == 0) stop("No rows left after filtering. Relax filters or check columns.")
  
  # Treatment indicator
  all_data$treat <- as.integer(all_data$Commune == treat_commune)
  
  # Window indicators (shock windows)
  all_data$shock1 <- as.integer(all_data$Date.mutation >= event1 & all_data$Date.mutation < (event1 + window_days))
  all_data$shock2 <- as.integer(all_data$Date.mutation >= event2 & all_data$Date.mutation < (event2 + window_days))
  
  # Outcome
  if (use_log) {
    all_data$y <- log(all_data$prix_m2)
    y_label <- "log(prix_m2)"
  } else {
    all_data$y <- all_data$prix_m2
    y_label <- "prix_m2"
  }
  
  # Build formula dynamically to avoid contrasts errors
  fit_one <- function(df, label) {
    
    add_factor_if_multi <- function(terms, df, varname) {
      if (varname %in% names(df) && length(unique(df[[varname]])) >= 2) {
        c(terms, paste0("factor(", varname, ")"))
      } else {
        terms
      }
    }
    
    rhs <- c("treat:shock1", "treat:shock2")
    rhs <- add_factor_if_multi(rhs, df, "Type.local")  # drops automatically in type-specific regressions
    rhs <- add_factor_if_multi(rhs, df, "quarter")
    rhs <- add_factor_if_multi(rhs, df, "Commune")
    
    f <- as.formula(paste("y ~", paste(rhs, collapse = " + ")))
    m <- lm(f, data = df)
    
    # Clustered SE (fallback to HC1 if only one cluster)
    if (!(cluster_var %in% names(df))) stop("cluster_var not found: ", cluster_var)
    
    if (length(unique(df[[cluster_var]])) < 2) {
      vc <- sandwich::vcovHC(m, type = "HC1")
      ct <- lmtest::coeftest(m, vcov. = vc)
      note <- paste0("NOTE: Only 1 unique ", cluster_var, "; used HC1 robust SE (not clustered).")
    } else {
      vc <- sandwich::vcovCL(m, cluster = df[[cluster_var]])
      ct <- lmtest::coeftest(m, vcov. = vc)
      note <- NULL
    }
    
    list(label = label, model = m, coeftest = ct, n = nrow(df), note = note)
  }
  
  out <- list(
    meta = list(
      outcome = y_label,
      treat_commune = treat_commune,
      event1 = event1,
      event2 = event2,
      window_days = window_days,
      cluster_var = cluster_var
    )
  )
  
  out$pooled <- fit_one(all_data, "Pooled (Appartement + Maison)")
  if (run_by_type) {
    out$appartement <- fit_one(subset(all_data, Type.local == "Appartement"), "Appartement only")
    out$maison      <- fit_one(subset(all_data, Type.local == "Maison"), "Maison only")
  }
  
  # Print compact summary
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
  
  cat("\nEvent-window DiD using ", out$meta$outcome, "\n", sep = "")
  cat("Treat commune: ", treat_commune, "\n", sep = "")
  cat("Event 1: ", as.character(event1), "  |  Event 2: ", as.character(event2), "\n", sep = "")
  cat("Window: ", window_days, " days\n", sep = "")
  cat("FE: quarter + commune (+ type in pooled) | Clustered SE by: ", cluster_var, "\n", sep = "")
  
  print_block(out$pooled)
  if (run_by_type) {
    print_block(out$appartement)
    print_block(out$maison)
  }
  
  invisible(out)
}