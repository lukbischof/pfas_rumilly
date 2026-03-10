# analysis/07_did_regressions.R
# ============================================================
# DiD regressions for Rumilly with TWO policy dates:
#   policy1: 2022-11-16
#   policy2: 2023-11-28
#
# Uses QUARTER fixed effects (recommended for small N monthly).
# Creates periods:
#   mid12 = [policy1, policy2)
#   post2 = [policy2, +inf)
#
# Primary parameters of interest:
#   treat:mid12  (policy 1 effect window)
#   treat:post2  (policy 2 effect window)
#
# Clustered SE by Commune using sandwich + lmtest.
# ============================================================

run_did_regressions <- function(all_data,
                                treat_commune = "RUMILLY",
                                policy1 = as.Date("2022-11-16"),
                                policy2 = as.Date("2023-11-28"),
                                use_log = TRUE,
                                cluster_var = "Commune",
                                time_fe = c("quarter"),   # keep simple: quarter
                                run_by_type = TRUE) {
  
  time_fe <- match.arg(time_fe)
  
  # Packages for clustered SE
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("Package 'sandwich' is required. Install with install.packages('sandwich').")
  }
  if (!requireNamespace("lmtest", quietly = TRUE)) {
    stop("Package 'lmtest' is required. Install with install.packages('lmtest').")
  }
  
  if (!is.data.frame(all_data)) stop("all_data must be a data.frame")
  
  # standardize names
  names(all_data) <- make.names(names(all_data))
  
  needed <- c("Date.mutation", "Commune", "Type.local", "prix_m2",
              "Surface.reelle.bati", "Valeur.fonciere")
  miss <- setdiff(needed, names(all_data))
  if (length(miss) > 0) {
    stop("Missing columns in all_data: ", paste(miss, collapse = ", "))
  }
  
  # Ensure Date.mutation is Date
  if (!inherits(all_data$Date.mutation, "Date")) {
    all_data$Date.mutation <- as.Date(as.character(all_data$Date.mutation), format = "%d/%m/%Y")
  }
  
  # Build quarter if missing
  if (!("quarter" %in% names(all_data)) || !inherits(all_data$quarter, "Date")) {
    m <- as.integer(format(all_data$Date.mutation, "%m"))
    q_start_month <- ((m - 1) %/% 3) * 3 + 1
    all_data$quarter <- as.Date(sprintf("%s-%02d-01", format(all_data$Date.mutation, "%Y"), q_start_month))
  }
  
  # Core filters (DVF hygiene)
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
  
  # Treatment and period indicators
  all_data$treat <- as.integer(all_data$Commune == treat_commune)
  all_data$mid12 <- as.integer(all_data$Date.mutation >= policy1 & all_data$Date.mutation < policy2)
  all_data$post2 <- as.integer(all_data$Date.mutation >= policy2)
  
  # Outcome
  if (use_log) {
    all_data$y <- log(all_data$prix_m2)
    y_label <- "log(prix_m2)"
  } else {
    all_data$y <- all_data$prix_m2
    y_label <- "prix_m2"
  }
  
  # Fixed effects
  fe_term <- "factor(quarter)"  # per argument; kept simple
  
  # -------- model builders --------
  fit_one <- function(df, label) {
    
    # Helper: include factor(x) only if x has >= 2 levels
    add_factor_if_multi <- function(terms, df, varname) {
      if (varname %in% names(df) && length(unique(df[[varname]])) >= 2) {
        c(terms, paste0("factor(", varname, ")"))
      } else {
        terms
      }
    }
    
    rhs_terms <- c("treat:mid12", "treat:post2")
    
    # Add controls / FE only when they have >=2 levels in this df
    rhs_terms <- add_factor_if_multi(rhs_terms, df, "Type.local")
    rhs_terms <- add_factor_if_multi(rhs_terms, df, "quarter")
    rhs_terms <- add_factor_if_multi(rhs_terms, df, "Commune")
    
    f <- as.formula(paste("y ~", paste(rhs_terms, collapse = " + ")))
    
    m <- lm(f, data = df)
    
    # Clustered SE (only if cluster var has >=2 levels; otherwise vcovCL will be singular/unhelpful)
    if (!(cluster_var %in% names(df))) stop("cluster_var not found: ", cluster_var)
    
    if (length(unique(df[[cluster_var]])) < 2) {
      # fallback: non-clustered robust SE (HC1) if only 1 cluster
      vc <- sandwich::vcovHC(m, type = "HC1")
      ct <- lmtest::coeftest(m, vcov. = vc)
      cluster_note <- paste0("NOTE: Only 1 unique ", cluster_var, "; used HC1 robust SE (not clustered).")
    } else {
      vc <- sandwich::vcovCL(m, cluster = df[[cluster_var]])
      ct <- lmtest::coeftest(m, vcov. = vc)
      cluster_note <- NULL
    }
    
    list(label = label, model = m, coeftest = ct, n = nrow(df), note = cluster_note)
  }
  
  # -------- run models --------
  out <- list()
  out$meta <- list(
    outcome = y_label,
    treat_commune = treat_commune,
    policy1 = policy1,
    policy2 = policy2,
    time_fe = time_fe,
    cluster_var = cluster_var
  )
  
  # Pooled
  out$pooled <- fit_one(all_data, "Pooled (Appartement + Maison)")
  
  # By type
  if (run_by_type) {
    out$appartement <- fit_one(subset(all_data, Type.local == "Appartement"), "Appartement only")
    out$maison      <- fit_one(subset(all_data, Type.local == "Maison"), "Maison only")
  }
  
  # -------- print a compact summary --------
  print_block <- function(res) {
    cat("\n===============================\n")
    cat(res$label, "\n")
    cat("N =", res$n, "\n")
    cat("===============================\n")
    # print only key coefficients
    ct <- res$coeftest
    keep <- rownames(ct) %in% c("treat:mid12", "treat:post2")
    print(ct[keep, , drop = FALSE])
  }
  
  cat("\nDiD regressions using ", out$meta$outcome, "\n", sep = "")
  cat("Treat commune: ", treat_commune, "\n", sep = "")
  cat("Policy1: ", policy1, "  |  Policy2: ", policy2, "\n", sep = "")
  cat("Time FE: quarter  |  Clustered SE by: ", cluster_var, "\n", sep = "")
  
  print_block(out$pooled)
  if (run_by_type) {
    print_block(out$appartement)
    print_block(out$maison)
  }
  
  invisible(out)
}