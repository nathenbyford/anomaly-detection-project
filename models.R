## Project models

library("tidyverse"); theme_set(theme_minimal())
library("ggdensity")
library("timetk")
library("isotree")
library("MLmetrics")
library("ANN2")

source("functions.R")

data <- read_csv("data/combined_data.csv")

glimpse(data)

names <- as.character(unique(data$company))

lm_results <- map_dfr(names,
  \(X) {
    results <- lm_inf_detection(filter(data, company == X))
    tibble(
      company = X,
      lm = results$AUC,
      tp_rate = results$true_pos
      )
    }
  ) 

lm_auc <- lm_results |> select(-tp_rate) |> 
  pivot_longer(cols = -company) |> 
  pivot_wider(names_from = company)

lm_tprate <- lm_results |> select(-lm) |> 
  rename(lm = tp_rate) |> 
  pivot_longer(cols = -company) |> 
  pivot_wider(names_from = company)


stl_results <- map_dfr(
  names,
  \(X) {
    results <- stl_detection(filter(data, company == X))
    tibble(
      company = X,
      stl = results$AUC,
      tp_rate = results$true_pos
    )
  }
) 

stl_auc <- stl_results |> 
  select(-tp_rate) |> 
  pivot_longer(cols = -company) |> 
  pivot_wider(names_from = company)

stl_tprate <- stl_results |> 
  select(-stl) |> 
  rename(stl = tp_rate) |> 
  pivot_longer(cols = -company) |> 
  pivot_wider(names_from = company)

isoForest_auc <- map_dfr(
  names,
  \(X) {
    results <- iso_forest_detection(filter(data, company == X))
    cbind(
      company = X,
      results$test
    )
  }
) |> pivot_longer(cols = -company) |> pivot_wider(names_from = company)

isoForest_tp <- map_dfr(
  names,
  \(X) {
    results <- iso_forest_detection(filter(data, company == X))
    cbind(
      company = X,
      results$true_pos
    )
  }
) |> pivot_longer(cols = -company) |> pivot_wider(names_from = company)


## Neural network

nn_res <- map_dfr(
  names,
  \(X) {
    results <- nn_detection(filter(data, company == X))
    tibble(
      company = X,
      SGD = results$AUC,
      tp_rate = results$true_pos
    )
  }
)

nn_auc <- nn_res |> select(-tp_rate) |> 
  pivot_longer(-company) |> 
  pivot_wider(names_from = company)

rbind(
  lm_auc,
  stl_auc,
  nn_auc,
  isoForest_auc
) |> 
  knitr::kable(digits = 4)

## True Positive rates

nn_tp <- nn_res |> 
  select(-SGD) |> 
  rename(SGD = tp_rate) |> 
  pivot_longer(-company) |> 
  pivot_wider(names_from = company)

rbind(
  lm_tprate,
  stl_tprate,
  nn_tp,
  isoForest_tp
) |> 
  knitr::kable(digits = 4)
