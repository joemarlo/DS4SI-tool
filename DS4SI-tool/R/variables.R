
# load data ---------------------------------------------------------------

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


# custom variables --------------------------------------------------------

population_n <- nrow(population_dataset)

# custom HTML code for collapsiible
#  see http://jsfiddle.net/thurstanh/emtAm/2/
HTML_for_collapsible_1 <- '<details><summary>View quantile</summary>'
HTML_for_collapsible_2 <- '</details><br>'

HTML_for_collapsible_Weighting_1 <- '<details><summary>View details</summary>'
HTML_for_collapsible_Weighting_2 <- '</details><br>'

# label for invitations_button_send
HTML_send_button <- paste0(
  'Send invitations<br>
  <p style="font-size: 0.6em; font-weight: 10;">
  Once sent, site selection will no longer be available</p>'
)

# violet color
violet_col <- "#5c5980"

# set list of variables for plotting options
categorical_vars <- sort(c('region', 'urban', 'other_prog'))
numeric_vars <- sort(c("unemp", "pct_hs", "income", "comfort", "cost"))

# choices for categorical selectInput
# order must match order of categorical_vars
categorical_choices <- list(
  rev(unique(as.character(population_dataset$other_prog))),
  unique(as.character(population_dataset$region)),
  rev(unique(as.character(population_dataset$urban)))
)

# create df of min and maxes to use in slider calculations
# convert this to base R
min_max_df <- population_dataset %>% 
  select(unemp, pct_hs, income, comfort, cost) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarize(min = floor(min(value) * 0.999 * 100) / 100,
            max = ceiling(max(value) * 1.001 * 100) / 100,
            .groups = "drop") %>%
  as.data.frame() %>% 
  `rownames<-`(.[,'name']) %>% 
  select(-name)
min_max_df['cost',] <- round(min_max_df['cost',], 0)

# sort the rows so its the same order as the numeric_vars
  # this is important because some of the renderUI elements are order-dependent
min_max_df <- min_max_df[numeric_vars,]
