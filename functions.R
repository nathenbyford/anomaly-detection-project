## Nate byford
## Functions for anomaly detection


lm_inf_detection <- function(data) {
  ## Using a linear model of time and values we are able to identify anomalous values
  ## from the influence points. This model takes into account if the given point has
  ## influence from on or all of the measures Cook's distance, Covariance ratio*, dffit*,
  ## df betas, and/or hat values.
  
  train_anomaly <- pull(data, anomaly)
  
  n_anom <- sum(train_anomaly)
  
  lm_mod <- lm(value ~ timestamp, data)
  
  inf_point <- as.data.frame(influence.measures(lm_mod)$is.inf)
  
  anom_ind <- inf_point |> 
    mutate(n = 1:nrow(inf_point)) |> 
    pivot_longer(-n) |> 
    group_by(n) |> 
    reframe(n_inf = sum(value)) |> 
    filter(n_inf > 0) |> 
    pull(n)
  
  anom_points <- left_join(
    data[, c(1, 2, 4)], 
    tibble(
      timestamp = data$timestamp[anom_ind], 
      anomaly_mod = TRUE
      ), 
    by = join_by(timestamp)
  )|> 
    mutate(anomaly_mod = if_else(is.na(anomaly_mod), FALSE, anomaly_mod))
  
  mod_anom <- anom_points |> 
    pull(anomaly_mod)
  
  mat <- table(mod_anom, train_anomaly)
  
  list(AUC = AUC(mod_anom, train_anomaly),
       conf_matrix = mat,
       data = anom_points,
       true_pos = mat[2, 2] / n_anom)
}


stl_detection <- function(data) {
  ## Using seasonal-trend decomposition procedure based on LOESS or STL, we can 
  ## identify anomalies. This is done with help from the timetk package.
  
  train_anomaly <- pull(data, anomaly)
  
  n_anom <- sum(train_anomaly)
  
  mod_anomalies <- data |> 
    anomalize(timestamp, value) |> 
    mutate(anomaly_model = anomaly == "Yes") |>
    select(timestamp, anomaly_model)
  
  anomaly_results <- data |> 
    select(timestamp, value, anomaly) |> 
    left_join(mod_anomalies, by = join_by(timestamp))
  
  model_anomaly <- pull(mod_anomalies, anomaly_model)
  
  mat <- table(model_anomaly, train_anomaly)
  
  list(AUC = AUC(model_anomaly, train_anomaly),
       conf_matrix = mat,
       data = anomaly_results,
       true_pos = mat[2, 2] / n_anom)
}

iso_forest_detection <- function(data) {
  train_anomaly <- pull(data, anomaly)
  data_without_anomaly <- select(data, -c(anomaly, company))
  
  iso_mod <- isolation.forest(
    data_without_anomaly,
    ndim = 1, sample_size=256,
    ntrees=100,
    missing_action="fail"
  )
  pred_orig <- predict(iso_mod, data_without_anomaly)
  
  model_dens <- isolation.forest(
    data_without_anomaly,
    ndim=1, sample_size=256,
    ntrees=100,
    missing_action="fail",
    scoring_metric="density"
  )
  pred_dens <- predict(model_dens, data_without_anomaly)
  
  model_fcf <- isolation.forest(
    data_without_anomaly,
    ndim=1, sample_size=32,
    prob_pick_pooled_gain=1,
    ntrees=100,
    missing_action="fail"
  )
  pred_fcf <- predict(model_fcf, data_without_anomaly)
  
  tibble(
    "Isolation Forest" = AUC(pred_orig, train_anomaly),
    "Density Isolation Forest" = AUC(pred_dens, train_anomaly),
    "Fair-Cut Forest" = AUC(pred_fcf, train_anomaly)
  )
}
