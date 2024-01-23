library(tidyverse)
library(tidyr)
full_data <-read_csv('MET2J_filtered_instruments.csv')

filtered_full_data <- full_data |>
  na.omit() |>
  filter(`Date of Birth`>= 1900, `Date of Birth` <= 2000 ) |>
  mutate(Decade = case_when(
    startsWith(as.character(`Date of Birth`), "190") ~ "1900s",
    startsWith(as.character(`Date of Birth`), "191") ~ "1910s",
    startsWith(as.character(`Date of Birth`), "192") ~ "1920s",
    startsWith(as.character(`Date of Birth`), "193") ~ "1930s",
    startsWith(as.character(`Date of Birth`), "194") ~ "1940s",
    startsWith(as.character(`Date of Birth`), "195") ~ "1950s",
    startsWith(as.character(`Date of Birth`), "196") ~ "1960s",
    startsWith(as.character(`Date of Birth`), "197") ~ "1970s",
    startsWith(as.character(`Date of Birth`), "198") ~ "1980s",
    startsWith(as.character(`Date of Birth`), "199") ~ "1990s",
  TRUE ~ as.character(`Date of Birth`)  )) |>
  group_by(Decade) |>
  select(-`Date of Birth`)
    
  
 
  
  
  
  
  
print(filtered_full_data)


