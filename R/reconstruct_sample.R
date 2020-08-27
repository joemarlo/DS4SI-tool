library(tidyverse)

load("/home/joemarlo/.cache/.fr-WYtiWz/Selection_history.RData")

# figure out which dataset was sent
stacked_results <- selection_history$data[[length(selection_history$data)]]
sent_invitations_IDs <- stacked_results$`Site ID`[stacked_results$`Site group` == 'Sent invitation']

# which dataset matches the sent invitations
index <- sapply(selection_history$data, function(IDs) base::setequal(IDs, sent_invitations_IDs))

# get the matching selections
selections <- selection_history$selections[index][[1]]

# get the page that the dataset was saved on
last_page <- selections$nav
last_page <- tolower(str_extract(last_page, "(Filtering|Sampling|Manual)"))

if (last_page == 'sampling'){
  if (selections$sampling_select_simple_or_stratified == 'Simple'){
    action <- paste("The student used simple sampling with n = ", sampling_slider_simple_n)
  } else {
    
    selections$strata_variables
    sliders <- grep(x = names(selections), pattern = paste0("^", last_page,"_slider_stratified"), value = TRUE)
    sliders_that_match <- 
    
    action <- paste(
      "The student used stratified sampling sampling with variables",
      paste0(selections$strata_variables, collapse = ', '),
      " and the following inputs: ",
      paste0()
  }
}

if (paste0(last_page, "_dataset") != "Population"){
  # do everything above again
}

# return the variables 
grep(x = names(selections), pattern = paste0("^", last_page), value = TRUE)

rm(selection_history)

sample_reset_sliders