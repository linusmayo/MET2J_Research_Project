library(tidyverse)
library(tidyr)
full_data <-read_csv('MET2J_filtered_instruments.csv')

filtered_full_data <- full_data |>
  na.omit() |>
  filter(`Date of Birth`>= )

print(filtered_full_data)


