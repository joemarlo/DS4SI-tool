library(tidyverse)

# this script recreates the student's selection history which lead to their final results
# it requires the Selecction_history.RData file downloaded on the sent invitations page
  # and the sites.csv file downloaded on the summary results page
# currently the script outputs the student's history in sentence format; this was done for 
  # my sanity so I could visualize if it was working or not. Further analysis may require
  # rectangular or some sort of hierarchial structure

# load the student's data
load("~/Desktop/Selection_history.RData")
student_selections <- selection_history
all_sites <- read_csv("~/Desktop/sites.csv")

# load the data containing all default variables
# this file was created by opening every page on the tool so Shiny would create
  # each possible variable combination (some inputs$ are lazy so they're only created
  # when needed). Then saving a dataset via each page, and then downloading the 
  # selection_history.RData file on the send invitations page
load("R/default_selection_history.RData")
default_selections <- selection_history
rm(selection_history)

# set the default filtering slider and select values
selections_filtering <- default_selections$selections[[1]]
default_filtering_names <- grep(x = names(selections_filtering), pattern = "^(filtering_select|filtering_slider)", value = TRUE)
default_filtering <- unlist(selections_filtering[default_filtering_names])


get_student_action <- function(selections) {
  
  # function returns the history of actions the student took on given page
  
  # get the page the dataset was saved on
  current_page <- selections$nav
  current_page <- tolower(str_extract(current_page, "(Filtering|Sampling|Manual)"))
  
  if (current_page == 'filtering') {

    # get just the filtering sliders and select inputs
    slider_names <- grep(
        x = names(selections),
        pattern = paste0("^", current_page, "_slider"),
        value = TRUE
      )
    select_names <- grep(
        x = names(selections),
        pattern = paste0("^", current_page, "_select"),
        value = TRUE
      )
    
    # get slider values and sort alphabetically
    slider_inputs <- unlist(selections[slider_names])
    slider_inputs <- slider_inputs[sort(names(slider_inputs))]
    names(slider_inputs) <- str_replace(names(slider_inputs), "1", "_min")
    names(slider_inputs) <- str_replace(names(slider_inputs), "2", "_max")

    # get default slider inputs
    default_slider_inputs <- default_filtering[grep(
        x = names(default_filtering),
        pattern = "^filtering_slider",
        value = TRUE
      )]
    default_slider_inputs <- default_slider_inputs[sort(names(default_slider_inputs))]
    names(default_slider_inputs) <- str_replace(names(default_slider_inputs), "1", "_min")
    names(default_slider_inputs) <- str_replace(names(default_slider_inputs), "2", "_max")
    
    # get the ones that don't match
    filtering_sliders_diff <- slider_inputs[!slider_inputs == default_slider_inputs]
    sliders_changed <- sapply(seq_along(filtering_sliders_diff), function(i){
        paste(
          names(filtering_sliders_diff)[i],
          unlist(filtering_sliders_diff[i]),
          sep = "_"
        )
      })
    
    # get the select inputs
    select_inputs <- unlist(selections[select_names])
    select_inputs <- select_inputs[sort(names(select_inputs))]
    
    # get default select inputs
    default_select_inputs <- default_filtering[grep(x = names(default_filtering), pattern = "^filtering_select", value = TRUE)]
    default_select_inputs <- default_select_inputs[sort(names(default_select_inputs))]
    
    # are either of the other prog values missing
    other_prog_missing <-
      c(TRUE, FALSE)[!c(TRUE, FALSE) %in% select_inputs[c(
        "filtering_select_other_program_at_site",
        "filtering_select_other_program_at_site1",
        'filtering_select_other_program_at_site2'
      )]]
    
    # are either of the urban values missing
    urban_missing <-
      c(TRUE, FALSE)[!c(TRUE, FALSE) %in% select_inputs[c(
        "filtering_select_urban",
        "filtering_select_urban1",
        'filtering_select_urban2'
      )]]
    
    # are any of the regions missing
    regions_missing <- c("Northeast", "Northcentral", "South", "West")[!c("Northeast", "Northcentral", "South", "West") %in% select_inputs]
    selects_missing <- list(other_prog = other_prog_missing, urban = urban_missing, region = regions_missing)
    selects_missing <- selects_missing[sapply(selects_missing, length) > 0]
    
    # separate into individual variables with their variable name
    selects_missing <- unlist(sapply(seq_along(selects_missing), function(i){
        paste(names(selects_missing)[i], unlist(selects_missing[i]), sep = "_")
    }))
    
    # paste the results into a coherent sentence
    action <-
      paste0(
        "The student used filtering with the following slider adjustments: ",
        paste0(sliders_changed, collapse = ", "),
        ". And removed the following categorical variables: ",
        paste0(selects_missing, collapse = ", ")
      )
    
  } else if (current_page == 'sampling') {
    if (selections$sampling_select_simple_or_stratified == 'Simple') {
      
      # TODO validate sampling inputs add up to dataframe
      
      action <- paste0(
        "The student used simple sampling with n = ",
        selections$sampling_slider_simple_n
        )
      
    } else {
      
      # TODO validate sampling inputs add up to dataframe
      
      # find the slider ids that match the entered strata variables
      all_stratified_sliders <- grep(x = names(selections),
             pattern = "^sampling_slider_stratified",
             value = TRUE)
      slider_names <- str_replace_all(paste0(selections$strata_variables, collapse = "_"), " ", "_")
      sliders_of_interest <- grep(x = all_stratified_sliders, pattern = slider_names, value = TRUE)
      
      # create combinations of [variable: n]
      slider_values <- paste0(sub(
        pattern = paste0(".*", slider_names, "_"),
        "",
        sliders_of_interest
      ),
      ": ",
      selections[sliders_of_interest])
      
      # paste all of it together
      action <- paste0(
        "The student used stratified sampling with variables ",
        paste0(selections$strata_variables, collapse = ', '),
        " and the following inputs: ",
        paste0(slider_values, collapse = "; ")
      )
    }
  } else if (current_page == 'manual') {
    action <- paste0(
      "The student used manual exclusions and excluded the following sites: ",
      paste0(selections$manual_select_sites_excl, collapse = ", ")
    )
    
  } else
    action <- "No page found"
 
  return(action) 
}


get_selection_history <- function(selection_history){
  
  # function wraps around "get_student_action()" so it can be applied
    # recursively given the starting dataset does not equal the population
  
  # figure out which dataset was sent
  sent_invitations_IDs <- all_sites$'Site ID'[all_sites$`Sent invitation`]
  
  # which dataset matches the sent invitations
  index <- sapply(selection_history$data, function(IDs) base::setequal(IDs, sent_invitations_IDs))
  
  # get the matching selections
  selections <- selection_history$selections[index][[1]]
  
  # get the page the dataset was saved on
  current_page <- selections$nav
  current_page <- tolower(str_extract(current_page, "(Filtering|Sampling|Manual)"))
  
  # get the selection history
  actions <- c()
  actions[1] <- get_student_action(selections)
  
  # if the last selection doesn't start with the Population dataset then we need
    # to figure out how that dataset was created
  dataset_started_with <- selections[paste0(current_page, "_dataset")]
  i <- 2
  while (dataset_started_with != "Population"){

    # get the selection history for that dataset
    selections <- selection_history$selections[selection_history$data_names == dataset_started_with][[1]]

    # do everything above again
    actions[i] <- get_student_action(selections)
    i <- i + 1
    
    # get the page the dataset was saved on
    current_page <- selections$nav
    current_page <- tolower(str_extract(current_page, "(Filtering|Sampling|Manual)"))

    # get the dataset the page started with
    dataset_started_with <- selections[paste0(current_page, "_dataset")]
  }

  # reverse the list so its chronological
  actions <- rev(actions)
  
  # add "step X: " to beginning of line so the order is obvious
  actions <- paste0(paste0("Step ", seq_along(actions), ": "), actions)
  
  return(actions)
}

# workflow is to load the student data then run the function:

# load students selection
load("~/Desktop/Selection_history.RData")
student_selections <- selection_history

# get the itemized selection history
get_selection_history(student_selections)

