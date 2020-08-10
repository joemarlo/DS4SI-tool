library(tidyverse) # for ggplot, dplyr data munging
library(shiny)
library(shinyWidgets) # for alerts
library(DT) # for UI tables
library(gridExtra) # for combining multiple ggplots into one object 
library(shinyjs) # for running javascript on the server side
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


# # messages for tooltips and popovers ------------------------------------

# order must match order of categorical_vars
categorical_popover_messages <- list(
  "Think about how the existence or absence of another jobs training program will impact the counterfactual and generalizability",
  "How will excluding regions affect the counterfactual and generalizability?",
  "How will excluding urban or rural sites affect the counterfactual and generalizability?"
)
# order must match order of numeric_vars
numeric_popover_messages <- list(
  "Comfort is a proxy for `probability of accepting the invitation` so you will have to approach more sites if comfort is low",
  "It is important to balance the goals of the evaluators along with the goals of the funders (i.e. keeping costs low)",
  "How does excluding low or high income sites affect the counterfactual and generalizability?",
  "How does excluding low or high high school graduation rates sites affect the counterfactual and generalizability?",
  "How does excluding low or high unemployment sites affect the counterfactual and generalizability?"
)

# message for sampling tooltip
sampling_message <- "A simple random sample is an unbiased surveying technique and, in expectation, retains the characteristics of the population. Stratified sampling enables you to partition subpopulations by variables and then sample the subpopulations separately."


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

# create dataframe of min and maxes to use in slider calculations
min_max_df <-
  t(sapply(population_dataset[, numeric_vars], function(col) {
    list(
      floor((min(col)*0.99) * 100) / 100, 
      ceiling((max(col)*1.01) * 100) / 100
    )
  }))
colnames(min_max_df) <- c("min", "max")
min_max_df <- as.data.frame(min_max_df)

# data to artificially set x limits in ggplot histograms
# this 'cheats' ggplot by creating a dataset of just the most extreme
  # observations. Then we pass this to ggplot as another layer that's invisible,
  # otherwise, setting limits on individual facets is a huge pain
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
