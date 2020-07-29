
scale_01 <- function(x) {
  # function scales the vector between 0 and 1
  (x - min(x)) / (max(x) - min(x))
}

get_quantile = function(distribution, value){
  # function to return the quantile amount
  
  # find the value in distribution that is closest to value
  closest_value_in_distribution <- distribution[which.min(abs(distribution - value))]
  
  # find the quantile of this closest value
  closest_quantile <- ecdf(distribution)(closest_value_in_distribution)
  return(closest_quantile)
} 

get_value <- function(distribution, probs){
  # function to get the quantile, but make sure it matches an actual value in the distribution
  # similar to stats::quantile() but returns the quantile to the closest value in the distribution
  
  # get the actual quantile
  actual_quantile <- as.numeric(quantile(distribution, probs = probs))
  
  # find the value in distribution that is closest to value
  closest_prob_in_distribution <- distribution[which.min(abs(distribution - actual_quantile))]
  
  # closest_value <- distribution[all_quantiles  == closest_quantile_in_distribution]
  
  return(closest_prob_in_distribution)
}

custom_datatable <- function(...){
  # wrapper around DT::datatable so commonly used arguments
  # can be set as global defaults
  
  DT::datatable(..., rownames = FALSE, 
                options = list(
                  # sets n observations shown
                  pageLength = 20,
                  # removes option to change n observations shown
                  lengthChange = FALSE,
                  # removes the search bar
                  sDom  = '<"top">lrt<"bottom">ip',
                  # enable side scroll so table doesn't overflow
                  scrollX = TRUE
                )
  )
}

draw_histograms <- function(data){
  
  # function takes in a dataset and then draws histograms for continuous
  # variables and barplots for categoricals. Returns a single grid.arrange
  # object to pass to renderPlot({})
  
  # bar plot of categorical variables
  p1 <- data %>%
    select(region, urban, other_prog) %>%
    mutate(urban = as.character(urban),
           other_prog = as.character(other_prog)) %>%
    pivot_longer(cols = everything()) %>%
    mutate(name = factor(name, levels = categorical_vars)) %>%
    ggplot(aes(x = value)) +
    geom_bar(fill = violet_col, alpha = 0.9) +
    # this ensures the x limits do not change by building an invisible
    # plot of the full population
    geom_bar(data = pop_data_for_categorical_limits,
             alpha = 0) +
    facet_wrap(~name, scales = 'free_x', ncol = 3) +
    labs(x = NULL,
         y = NULL) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  
  # histograms plot of numeric variables
  p2 <- data %>%
    select(unemp, pct_hs, income, comfort, cost) %>%
    pivot_longer(cols = everything()) %>%
    mutate(name = factor(name, levels = numeric_vars)) %>%
    ggplot(aes(x = value)) +
    geom_histogram(fill = violet_col, alpha = 0.9, color = 'white', bins = 20) +
    # this ensures the x limits do not change by building an invisible
    # plot of the full population
    geom_histogram(data = pop_data_for_numeric_limits,
                   alpha = 0, bins = 20) +
    facet_wrap(~name, scales = 'free_x', ncol = 3) +
    labs(x = NULL,
         y = NULL)
  
  # render both plots vertically
  grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
  
}


score_attributes <- function(data){
  
  # function returns a table of the score attributes
  # total cost, generalizability, causality, sample size
  
  # total cost
  total_cost <- sum(data$cost)
  total_cost <- scales::dollar(total_cost)
  
  # generalizability TBD
  general_score <- round(score_generalizability(data), 2)
  
  # causality
  causality_score <- as.character("NA")
  
  # sample size
  n_sites <- nrow(data)
  n_sites <- scales::comma(n_sites)
  
  # build the final table
  metrics_table <- data.frame(Metric = c(total_cost, general_score, causality_score, n_sites))
  rownames(metrics_table) <- c("Total cost", "Generalizability score", "Causality score", "Sample size")
  
  # return the table
  return(metrics_table)
  
}

n_sites_text <- function(data){
  # function to output HTML text indicating
  # how many rows are a dataframe
  
  paste0('<h4>',
         nrow(data),' 
        sites are currently selected to be approached</h4>'
  )
}


determine_x_pos <- function(value){
  # for ggplot
  # determines x position for drawing horizontal 
    # segments while grouping 
  
  case_when(
    value == FALSE ~ 1,
    value == TRUE ~ 2,
    value == "Northcentral" ~ 1,
    value == "Northeast" ~ 2,
    value == "South" ~ 3,
    value == "West" ~ 4
  )
}

show_alert_min_sites <- function(session = session) {
  # function is a wrapper around show_alert()
  # sets the default message for the popups indicating
    # the minimum amount of sites has not been reached
  show_alert(
    title = paste0(
      "Less than ",
      min_sites_to_approach,
      " sites are currently selected"
    ),
    text = paste0("You must approach at least ", min_sites_to_approach, " sites"),
    type = "warning",
    btn_colors = "#302f42",
    session = session
  )
}
