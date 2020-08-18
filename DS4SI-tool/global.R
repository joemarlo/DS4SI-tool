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


# create score_causality() ------------------------------------------------

# create df of just site_id
causal_scores <- population_dataset[, 'site_id']

# add causal score to the df
# score is a 4 for sites without other_prog and 1 with
# add rnorm noise with sd = 0.35
# make sure score is not below 0
causal_scores$causal_score <- pmax(0, rnorm(
  n = nrow(causal_scores),
  mean = ((!population_dataset$other_prog) * 3) + 1,
  sd = 0.35
))

score_causality <- function(data, scores_df = causal_scores){
  # function returns a single numeric between [0, 1]
    # that represents the causal score
  # 1 indicates a perfect score
  
  # get the causal scores per sites that are selected
  selected_sites <- left_join(data, scores_df, by = 'site_id')
  
  # define the best possible and worst possible scores as the means of the 
    # top 100 and bottom 100 site scores
  # best_sites <- scores_df[order(-scores_df$causal_score),][1:min_sites_to_approach,]
  # best_score <- mean(best_sites$causal_score)
  # worst_sites <- scores_df[order(scores_df$causal_score),][1:min_sites_to_approach,]
  # worst_score <- mean(worst_sites$causal_score)
  
  # define the user's score by taking the average score and then
    # scaling it between the top and worst scores
  score <- mean(selected_sites$causal_score)
  # scaled_score <- (score - worst_score) / (best_score - worst_score)
  
  return(score)
}

# testing
# scores <- sapply(rep(100, 5000), function(i){
#   boolean <- sample(list(TRUE, FALSE, c(TRUE, FALSE)), size = 1) %>% unlist()
#   slice_sample(population_dataset[population_dataset$other_prog %in% boolean,], n = i) %>% score_causality()
# })
# 
# summary(scores)
# plot(density(scores))

# # messages for tooltips and popovers ------------------------------------

# order must match order of categorical_vars
categorical_popover_messages <- list(
  "Think about how the existence or absence of another jobs training program could impact the size of the treatment effect.",
  "How will excluding regions affect the generalizability?",
  "How will excluding urban or rural sites affect generalizability?"
)
# order must match order of numeric_vars
numeric_popover_messages <- list(
  "Comfort is a proxy for `probability of accepting the invitation` so you will have to approach more sites if average comfort is low.",
  "It is important to balance the goals of the evaluators along with the goals of the funders (i.e. keeping costs low).",
  "How does excluding low or high income sites affect the generalizability?",
  "How does excluding low or high high school graduation rates sites affect generalizability?",
  "How does excluding low or high unemployment sites affect generalizability?"
)

# message for sampling tooltip
sampling_simple_message <- "A simple random sample from the population should, on average, retain the characteristics of the population."
sampling_stratified_message <- "Stratified sampling allows you to control the probability of being selected seperately within different groups of sites. This may be helpful if the probability of accepting the invitation varies across your strata."

# message for run simulation button
results_message_sim_button <- "Resend the invitations 250 times and see how the distribution of these results compare to your final sample"

# message for floating window on results page
results_message_float <- list(
  strong("This is how your final sample compares to 250 random draws"),
  br(),br(),
  HTML("If you resent the invitations 250 times, these are the distributions of those outcomes. The vertical line represents your final sample.")
)

# misc variables ----------------------------------------------------------

# preset numeric and categorical variables; this is used widely in ui.R
  # and server.R; its important they remain alphabetical
numeric_vars <- sort(c("unemp", "pct_hs", "income", "comfort", "cost"))
categorical_vars <- sort(c('region', 'urban', 'other_prog'))

# set order of metrics (used for plots)
metrics_order <- c("sample_size", "total_cost", "generalizability_index", "causality_index")

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
