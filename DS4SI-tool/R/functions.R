
scale_01 <- function(x) {
  # function scales the vector between 0 and 1
  (x - min(x)) / (max(x) - min(x))
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
  
  # sample size
  n_sites <- round(nrow(data))
  n_sites <- scales::comma(n_sites)
  
  # total cost
  total_cost <- round(sum(data$cost))
  total_cost <- scales::dollar(total_cost)
  
  # generalizability score
  generalizability_index <- round(score_generalizability(data), 2)
  
  # causality
  causality_index <- round(score_causality(data), 2)
  
  # build the final table
  metrics_table <- data.frame(Metric = c(n_sites, total_cost, generalizability_index, causality_index))
  rownames(metrics_table) <- c("Sample size", "Total cost", "Generalizability index", "Causality index")
  
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

get_dataset <- function(dataset_name, list_of_datasets){
  # function returns a dataset based on its name from the 
    # pre-defined reactiveList
  # the function is only (currently) used to access 2. dataframes saved by the user
    # and 2. the 'stacked_results' dataframe
  
  # get list of site_ids
  site_ids <- list_of_datasets$data[[match(dataset_name, list_of_datasets$data_names)]]
  
  # if the input name is "stacked_results" then return back the object as this 
    # is a dataframe, not just site_ids like all other items stored in the list
  # "stacked_results" is created when the user clicks input$invitations_popup_confirm
  if (dataset_name == "stacked_results"){
    return(site_ids)
  }
  
  # filter the population dataframe to just include those site_ids
  data <- population_dataset[population_dataset$site_id %in% site_ids,]
  
  return(data)
}

