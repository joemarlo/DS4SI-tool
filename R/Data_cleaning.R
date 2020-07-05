library(tidyverse)


original_df <- read.csv("Data/jpta.csv")

#clean up dataframe
cleaned_df <- original_df %>%
  mutate(
    region = case_when(
      region == 1 ~ "Northeast",
      region == 2 ~ "Northcentral",
      region == 3 ~ "South",
      region == 4 ~ "West"
    ),
    urban = as.logical(urban),
    other_prog = as.logical(other_prog),
    cost = round(1000 + (distance * 500), 0)
  ) %>% 
  select(-distance) %>% 
  mutate_if(is.numeric, round, digits = 3)

write_csv(cleaned_df, "DS4SI-tool/jpta_cleaned.csv")
