## Project models

library("tidyverse"); theme_set(theme_minimal())
library("ggdensity")
library("timetk")
library("isotree")
library("MLmetrics")

source("functions.R")

data <- read_csv("data/combined_data.csv")

glimpse(data)

apple <- data |> filter(company == "AAPL")

names <- as.character(unique(data$company))

lm_auc <- map_dfr(names,
  ~tibble(
    company = .,
    lm = lm_inf_detection(filter(data, company == .))$AUC,
    )
  )
lm_tprate <- map_dfr(names,
  ~tibble(
    company = .,
    lm = lm_inf_detection(filter(data, company == .))$true_pos,
    )
  )


stl_results <- map_dfr(
  names,
  ~tibble(
    company = .,
    stl = stl_detection(filter(data, company == .))$AUC
  )
)
stl_tprate <- map_dfr(
  names,
  ~tibble(
    company = .,
    stl = stl_detection(filter(data, company == .))$true_pos
  )
)

isoForest_results <- map_dfr(
  names,
  ~cbind(
    company = .,
    iso_forest_detection(filter(data, company == .))
  )
)

left_join(lm_results, stl_results, by = join_by(company)) |> 
  left_join(isoForest_results, by = join_by(company)) |> 
  knitr::kable(digits = 4)
