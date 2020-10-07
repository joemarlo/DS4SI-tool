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
  "data/jpta.csv",
  col_types = cols(
    `Site ID` = col_integer(),
    Region = col_character(),
    Urban = col_logical(),
    `Other program at site` = col_logical(),
    `Unemployment rate` = col_double(),
    `High school degree rate` = col_double(),
    `Mean income` = col_double(),
    Comfort = col_double(),
    `Cost to approach site` = col_double(),
    `Cost to run study` = col_double()
  )
)


# load score_generalizbility(), score_causality(), and individual  --------

load('R/score_generalizability.RData')
load('R/score_causality.RData')
load('R/score_causality_individuality.RData')

# # messages for tooltips and popovers ------------------------------------

# order must match order of categorical_vars
categorical_popover_messages <- list(
  "Think about how the existence or absence of another jobs training program could impact the size of the treatment effect (i.e. causality). If there is a competing program, we expect the effect to be much smaller.",
  "How will excluding regions affect generalizability?",
  "How will excluding urban or rural sites affect generalizability?"
)
# order must match order of numeric_vars
numeric_popover_messages <- list(
  "Comfort is a proxy for `probability of accepting the invitation` so you will have to approach more sites if average comfort is low.",
  "It is important to balance the goals of the evaluators along with the goals of the funders (i.e. keeping costs low).",
  "It is important to balance the goals of the evaluators along with the goals of the funders (i.e. keeping costs low).",
  "How does excluding low or high high school graduation rates sites affect generalizability?",
  "How does excluding low or high income sites affect generalizability?",
  "How does excluding low or high unemployment sites affect generalizability?"
)

# message for sampling tooltip
sampling_simple_message <- "A simple random sample from the population should, on average, retain the characteristics of the population."
sampling_stratified_message <- "Stratified sampling allows you to control the probability of being selected within different groups of sites. This may be helpful if the probability of accepting the invitation varies across your strata."

# message for floating window on results page
invitations_plot_att_box_message <- list(
  strong("This is the expected range of outcomes for your selected sample"),
  br(), br(),
  HTML("If you sent the invitations 200 times, each line represents the density of one of these outcomes"),
  br(), br()
)
invitations_plot_metrics_box_message <- list(
  strong("This is the expected range of outcomes for your selected sample"),
  br(), br(),
  HTML("If you sent the invitations 200 times, these are the distributions of those outcomes' metrics"),
  br(), br()
)


# misc variables ----------------------------------------------------------

# preset numeric and categorical variables; this is used widely in ui.R
  # and server.R; its important they remain alphabetical
categorical_vars <- sort(c("Region", "Urban", "Other program at site"))
numeric_vars <- sort(c("Unemployment rate", "High school degree rate", "Mean income", 
       "Comfort", "Cost to approach site", "Cost to run study"))

# set order of metrics (used for plots)
metrics_order <- c("Sample size", "Total cost", "Generalizability index", "Causality index")

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
  sort(unique(as.character(population_dataset$`Other program at site`))),
  sort(unique(as.character(population_dataset$Region))),
  sort(unique(as.character(population_dataset$Urban)))
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
  select(all_of(numeric_vars)) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name, levels = numeric_vars)) %>%
  group_by(name) %>% 
  filter(value == min(value) | value == max(value))
pop_data_for_categorical_limits <-
  tibble(
    'Region' = unique(population_dataset$Region),
    'Urban' = rep(unique(population_dataset$Urban), 2),
    'Other program at site' = rep(unique(population_dataset$`Other program at site`), 2)
  ) %>%
  mutate(Urban = as.character(Urban),
         `Other program at site` = as.character(`Other program at site`)) %>%
  pivot_longer(cols = everything())

# list of the prefixes for saving datasets on each page
# prefix_name = id of the inputText of the dataset name
# prefix_button = id of the actionButton to save the dataset
data_save_name_prefixes <- c(
  "filtering_data_save",
  "sampling_data_save",
  "manual_data_save"
)

# list of dropdown ids
dataset_selector_ids <- c(
  "filtering_dataset", 
  "sampling_dataset", 
  "manual_dataset",
  "invitations_dataset",
  "exploration_dataset"
)
