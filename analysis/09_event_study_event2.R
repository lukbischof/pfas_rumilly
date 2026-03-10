library(ggplot2)
library(lmtest)
library(sandwich)

run_event_study_event2 <- function(all_data,
                                   treat_commune = "RUMILLY",
                                   event_date = as.Date("2023-11-28"),
                                   min_q = -4,
                                   max_q = 4) {
  
  df <- all_data
  
  # treatment indicator
  df$treat <- ifelse(df$Commune == treat_commune, 1, 0)
  
  # event time in quarters
  df$event_time <- floor(
    as.numeric(df$Date.mutation - event_date) / 90
  )
  
  # keep reasonable range
  df <- subset(df, event_time >= min_q & event_time <= max_q)
  
  df$event_time <- factor(df$event_time)
  
  # regression
  model <- lm(
    log(prix_m2) ~ treat:event_time +
      factor(quarter) +
      factor(Commune),
    data = df
  )
  
  # clustered SE
  vc <- vcovCL(model, cluster = df$Commune)
  res <- coeftest(model, vcov = vc)
  
  # extract coefficients
  coefs <- data.frame(
    term = rownames(res),
    estimate = res[,1],
    se = res[,2]
  )
  
  coefs <- subset(coefs, grepl("treat:event_time", term))
  
  coefs$event_time <- as.numeric(gsub("treat:event_time", "", coefs$term))
  
  # confidence intervals
  coefs$lower <- coefs$estimate - 1.96 * coefs$se
  coefs$upper <- coefs$estimate + 1.96 * coefs$se
  
  # plot
  p <- ggplot(coefs, aes(event_time, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
    geom_vline(xintercept=0, linetype="dashed") +
    geom_hline(yintercept=0, linetype="dashed") +
    labs(
      x = "Quarters relative to contamination news",
      y = "Log price effect (Rumilly vs controls)",
      title = "Event study: groundwater contamination news"
    ) +
    theme_minimal()
  
  print(p)
  
}