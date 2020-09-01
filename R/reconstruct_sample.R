library(tidyverse)
setwd("DS4SI-tool")

# get default data
source("global.R")
rm(list = setdiff(ls(), c("min_max_df", "population_dataset")))

# load the student's data
load("~/Desktop/Selection_history.RData")
all_sites <- read_csv("~/Desktop/sites.csv")

# figure out which dataset was sent
sent_invitations_IDs <- all_sites$'Site ID'[all_sites$`Sent invitation`]

# which dataset matches the sent invitations
index <- sapply(selection_history$data, function(IDs) base::setequal(IDs, sent_invitations_IDs))

# get the matching selections
selections <- selection_history$selections[index][[1]]

selections <- selection_history$selections[[1]]

# get the page that the dataset was saved on
last_page <- selections$nav
last_page <- tolower(str_extract(last_page, "(Filtering|Sampling|Manual)"))

# last_page <- 'filtering'


if (last_page == 'filtering'){
  
  # only find sliders that do not have changes from the default
  
  # get just the filtering sliders and select inputs
  # slider_select_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_s"), value = TRUE)
  slider_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_slider"), value = TRUE)
  select_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_select"), value = TRUE)
  
  # get slider values and sort alphabetically
  slider_inputs <- unlist(selections[slider_names])
  slider_inputs <- slider_inputs[sort(names(slider_inputs))]
  names(slider_inputs) <- str_replace(names(slider_inputs), "1", "_min")
  names(slider_inputs) <- str_replace(names(slider_inputs), "2", "_max")
  
  # get the default slider settings
  min_max_df$min <- unlist(min_max_df$min)
  min_max_df$max <- unlist(min_max_df$max)
  defaults_list <- as.list(t(as.matrix(min_max_df)))
  names(defaults_list) <- names(slider_inputs) # this works because everything is sort alpha

  # get select values and sort alphabetically
  select_inputs <- unlist(selections[select_names])
  select_inputs <- select_inputs[sort(names(select_inputs))]

  # check to see which don't match
  sliders_changed <- slider_inputs[slider_inputs == defaults_list]
  names(sliders_changed) <- str_remove_all(names(sliders_changed), "filtering_slider_")
    
  # are either of the other prog values missing
  other_prog_missing <- c(TRUE, FALSE)[!c(TRUE, FALSE) %in% select_inputs[c("filtering_select_other_program_at_site", "filtering_select_other_program_at_site1", 'filtering_select_other_program_at_site2')]]
  
  # are either of the urban values missing
  urban_missing <- c(TRUE, FALSE)[!c(TRUE, FALSE) %in% select_inputs[c("filtering_select_urban", "filtering_select_urban1", 'filtering_select_urban2')]]
  
  # are any of the regions missing 
  regions_missing <- c("Northeast", "Northcentral", "South", "West")[!c("Northeast", "Northcentral", "South", "West") %in% select_inputs]
  
  # TODO
  list(other_prog_missing, urban_missing, regions_missing)
  
  sliders_changed
  
  


  action <- paste0("The student used filtering with ")
  
} else if (last_page == 'sampling'){
  if (selections$sampling_select_simple_or_stratified == 'Simple'){
    action <- paste0("The student used simple sampling with n = ", selections$sampling_slider_simple_n)
  } else {
    
    # find the slider ids that match the entered strata variables
    all_stratified_sliders <- grep(x = names(selections), pattern = "^sampling_slider_stratified", value = TRUE)
    slider_names <- str_replace_all(paste0(selections$strata_variables, collapse = "_"), " ", "_")
    sliders_of_interest <- grep(x = all_stratified_sliders, pattern = slider_names, value = TRUE)
    
    # create combines of varible: n
    slider_values <- paste0(
      sub(pattern = paste0(".*", slider_names, "_"), "", sliders_of_interest),
      ": ", 
      selections[sliders_of_interest]
    )
    
    # paste all of it together
    action <- paste0(
      "The student used stratified sampling sampling with variables ",
      paste0(selections$strata_variables, collapse = ', '),
      " and the following inputs: ",
      paste0(slider_values, collapse = "; ")
    )
  }
} else if (last_page == 'manual') {
  
} else action <- "No page found"


if (paste0(last_page, "_dataset") != "Population"){
  # do everything above again
}




# return the variables 
grep(x = names(selections), pattern = paste0("^", last_page), value = TRUE)

rm(selection_history)
