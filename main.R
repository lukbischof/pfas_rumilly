# =====================================================
# main.R
# Main entry point of the project
#currently used packages (as of 04.03.2026): ggplot2, sandwich, lmtest
# =====================================================

# 1) set working directory
setwd("/home/wsl/AEER") # ADJUST THIS
# make sure working directory is AEER
project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# 2) load data
source("data_selector.R")

# load selected datasets
out <- load_selected_data()
all_data <- out$all_data

# inspect
str(all_data)
head(all_data)

# open viewer (optional)
View(head(all_data, 1000))

# 3) add important metrics

# Compute price per square meter & filter for observations with sensible data
all_data$prix_m2 <- all_data$Valeur.fonciere / all_data$Surface.reelle.bati
all_data <- subset(
  all_data,
  !is.na(prix_m2) &
    prix_m2 > 500 & prix_m2 < 15000 &
    !is.na(Surface.reelle.bati) &
    Surface.reelle.bati >= 10 &
    !is.na(Valeur.fonciere) &
    Valeur.fonciere > 50000 & Valeur.fonciere < 2000000 &
    Type.local %in% c("Appartement", "Maison") &
    (is.na(Surface.terrain) | Surface.terrain < 5000)
)

message("After filters, N = ", nrow(all_data))

# Month (first day of month)
all_data$month <- as.Date(format(all_data$Date.mutation, "%Y-%m-01"))

# Quarter (first day of quarter)
# Example: 2022-05-17 -> 2022-04-01
q <- as.integer((as.integer(format(all_data$Date.mutation, "%m")) - 1) / 3)
all_data$quarter <- as.Date(sprintf("%s-%02d-01",
                                    format(all_data$Date.mutation, "%Y"),
                                    q * 3 + 1))

# 4) create output folders
dir.create("analysis", showWarnings = FALSE)
dir.create(file.path("outputs", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)

plot_dir  <- file.path("outputs", "plots")
table_dir <- file.path("outputs", "tables")

# 4.1) Delete plots at the end (for now)
on.exit({
  files <- list.files(plot_dir, full.names = TRUE)
  if (length(files)) unlink(files, recursive = TRUE, force = TRUE)
  message("Cleaned plots folder (deleted generated graphs).")
}, add = TRUE)

# 5) Run analyses
source(file.path("analysis", "01_scatter_prix_m2_month.R"))
source(file.path("analysis", "02_monthly_counts_table.R"))
source(file.path("analysis", "03_monthly_avg_price_lines.R"))
source(file.path("analysis", "04_quarterly_price_lines.R"))
source(file.path("analysis", "05_monthly_rumilly_vs_others.R"))
source(file.path("analysis", "06_quarterly_rumilly_vs_others.R"))
source(file.path("analysis", "07_did_regressions.R"))
source(file.path("analysis", "08_did_event_windows.R"))
source(file.path("analysis", "09_event_study_event2.R"))
source("analysis/10_did_controls.R")
source("analysis/11_property_composition.R")
source("analysis/12_did_event_windows_controls.R")
source("analysis/13_transaction_volume_maison_poisson.R")

make_scatter_prix_m2_month(all_data, plot_dir)
counts_tbl <- make_monthly_counts_table(all_data, table_dir)
make_monthly_price_lines(all_data, plot_dir, stat = "median")
make_quarterly_price_lines(all_data, plot_dir, min_n = 5, stat = "median")
make_monthly_rumilly_vs_others(all_data, plot_dir, stat = "median")
make_quarterly_rumilly_vs_others(all_data, plot_dir, min_n = 5, stat = "median")
comp11 <- plot_transaction_composition(all_data, plot_dir, table_dir)

#regression 07_
did_out <- run_did_regressions(
  all_data,
  treat_commune = "RUMILLY",
  policy1 = as.Date("2022-11-16"),
  policy2 = as.Date("2023-11-28"),
  use_log = TRUE,
  cluster_var = "Commune",
  run_by_type = TRUE
)

#regression 08_ (assuming effect only holds for some specified time period)
WINDOW_DAYS <- 365   # try 180, 270, 365 as sensitivity checks

did08 <- run_event_window_did(
  all_data,
  treat_commune = "RUMILLY",
  event1 = as.Date("2022-11-16"),
  event2 = as.Date("2023-11-28"),
  window_days = WINDOW_DAYS,
  use_log = TRUE,
  cluster_var = "Commune",
  run_by_type = TRUE
)

#regression 09_ (show dynamic effect for houses only)
run_event_study_event2(
  subset(all_data, Type.local == "Maison")
)

#regression 10_ (property controls)
did_controls <- run_did_controls(all_data)
run_did_controls(subset(all_data, Type.local == "Maison"))
run_did_controls(subset(all_data, Type.local == "Appartement"))

#regression 12_ (property controls with considering effect only for some specified time period)
res <- run_event_window_did_controls(
  all_data = all_data,
  treat_commune = "RUMILLY",
  event1 = as.Date("2022-11-16"),
  event2 = as.Date("2023-11-28"),
  window_days = 365,
  use_log = TRUE,
  cluster_var = "Commune",
  run_by_type = TRUE
)

#regression 13 (transaction volume including 365 window and property controls)
did13 <- run_transaction_volume_poisson_maison(
  all_data,
  treat_commune = "RUMILLY",
  event1 = as.Date("2022-11-16"),
  event2 = as.Date("2023-11-28"),
  window_days = 365
)