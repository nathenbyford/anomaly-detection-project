## Nathen Byford
## GAN model for anomaly detection

library("tidyverse"); theme_set(theme_minimal())
library("keras")
library("RGAN")

data <- read_csv("data/combined_data.csv")

facebook <- data |> filter(company == "FB") |> 
  select(timestamp, value) |> 
  mutate(x = as.numeric(timestamp), y = value) |> 
  select(x, y)

train_x <- array_reshape(facebook, 
                         c(nrow(facebook), nrow(facebook)))

data_transformer$new(facebook)
