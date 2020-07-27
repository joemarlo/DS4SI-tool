library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)
library(gridExtra)
library(shinyjs)
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

# population_dataset <- read_csv("data/jpta.csv")
numeric_vars <- sort(c("unemp", "pct_hs", "income", "comfort", "cost"))
categorical_vars <- sort(c('region', 'urban', 'other_prog'))

# total number of sites in the population
population_n <- nrow(population_dataset)

# label for invitations_button_send
HTML_send_button <- paste0(
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

# create df of min and maxes to use in slider calculations
min_max_df <-
  t(sapply(population_dataset[, numeric_vars], function(col)
    list(min(col), max(col))))
colnames(min_max_df) <- c("min", "max")
min_max_df <- as.data.frame(min_max_df)

      