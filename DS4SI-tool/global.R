library(tidyverse)
library(shiny)
library(shinyWidgets) # for alerts
library(DT) # for UI tables
library(gridExtra) # for combining multiple ggplots into one object 
library(shinyjs)
library(shinyBS) # for popovers
library(viridis) # for better colors for color blind people
set.seed(44)


# dataset -----------------------------------------------------------------

population_dataset <- read_csv(
  file = "data/jpta.csv",
  col_types = cols(
    site_id = col_double(),
    region = col_character(),
    urban = col_logical(),
    other_prog = col_logical(),
    unemp = col_double(),
    pct_hs = col_double(),
    income = col_double(),
    comfort = col_double(),
    cost = col_double()
  )
)


# load score_generalizbility() --------------------------------------------

load('R/score_generalizability.RData')


# misc variables ----------------------------------------------------------

# preset numeric and categorical variables; this is used widely in ui.R
  # and server.R; its important they remain alphabetical
numeric_vars <- sort(c("unemp", "pct_hs", "income", "comfort", "cost"))
categorical_vars <- sort(c('region', 'urban', 'other_prog'))

# total number of sites in the population
population_n <- nrow(population_dataset)

# minimum amount of sites to approach
min_sites_to_approach <- 100

# label for invitations_button_send
invitations_HTML_send <- paste0(
  'Send invitations<br>
  <p style="font-size: 0.6em; font-weight: 10;">
  Once sent, site selection will no longer be available</p>'
)

# violet color
violet_col <- "#5c5980"

# choices for categorical selectInput
# order must match order of categorical_vars
categorical_choices <- list(
  rev(unique(as.character(population_dataset$other_prog))),
  unique(as.character(population_dataset$region)),
  rev(unique(as.character(population_dataset$urban)))
)

# messages for popovers
# order must match order of categorical_vars
categorical_popover_messages <- list(
  "Remember that ...",
  "Focusing on one region may...",
  "Only including urban may reduce..."
)
# order must match order of numeric_vars
numeric_popover_messages <- list(
  "Remember that ...",
  "Focusing on one region may...",
  "Only including urban may reduce...",
  "however ...",
  "this is gonna be good"
)

# create df of min and maxes to use in slider calculations
min_max_df <-
  t(sapply(population_dataset[, numeric_vars], function(col) {
    list(
      floor((min(col)*0.99) * 10) / 10, 
      ceiling((max(col)*1.01) * 10) / 10
    )
  }))
colnames(min_max_df) <- c("min", "max")
min_max_df <- as.data.frame(min_max_df)

# data to artificially set x limits in ggplot histograms
pop_data_for_numeric_limits <- population_dataset %>% 
  select(unemp, pct_hs, income, comfort, cost) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = numeric_vars)) %>%
  group_by(name) %>% 
  filter(value == min(value) | value == max(value))
pop_data_for_categorical_limits <-
  tibble(
    'region' = unique(population_dataset$region),
    'urban' = rep(unique(population_dataset$urban), 2),
    'other_prog' = rep(unique(population_dataset$other_prog), 2)
  ) %>%
  mutate(urban = as.character(urban),
         other_prog = as.character(other_prog)) %>%
  pivot_longer(cols = everything())