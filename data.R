## Nathen Byford
## Numenta Anomaly Benchmark Data

library("tidyverse"); theme_set(theme_bw())
library("jsonlite")

csv_files <- list.files(path = "data", pattern = "Twitter_volume_.*\\.csv")

stocks <- str_extract(csv_files, "(?<=_)\\w{2,4}(?=\\.csv)")

data_list <- map(csv_files, 
                 function(x) {
                   read_csv(file.path(path = "data", x)) |> 
                     mutate(company = str_extract(x, "(?<=_)\\w{2,4}(?=\\.csv)"))
                   }
                 )

# data_list[1] |> glimpse()

labels <- fromJSON("data/tweet_labels.json")

labels <- map(labels, ~tibble(timestamp = as_datetime(.x), anomaly = TRUE))

data_list <- map(1:10, ~left_join(data_list[[.x]], labels[[.x]]))

data <- do.call(rbind, data_list)

data <- data |> mutate(anomaly = if_else(is.na(anomaly), FALSE, TRUE))

write_csv(data, file = "data/combined_data.csv")

