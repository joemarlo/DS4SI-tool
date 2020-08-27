library(tidyverse)

# get default data
source("DS4SI-tool/global.R")
rm(list = setdiff(ls(), c("min_max_df", "population_dataset")))

# load the student's data
#load("/home/joemarlo/.cache/.fr-Bo636w/Selection_history.RData")
load("/home/joemarlo/.cache/.fr-AclZQx/Selection_history.RData")

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

last_page <- 'filtering'

if (last_page == 'filtering'){
  
  # only find sliders that do not have changes from the default
  
  # get just the filtering sliders and select inputs
  # slider_select_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_s"), value = TRUE)
  slider_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_slider"), value = TRUE)
  select_names <- grep(x = names(selections), pattern = paste0("^", last_page, "_select"), value = TRUE)
  
  # munge the data to get it into the same format
  slider_inputs_df <- as.data.frame(t(as.data.frame(selections[slider_names])))
  colnames(slider_inputs_df) <- c("min", "max")
  slider_inputs_df <- slider_inputs_df[order(rownames(slider_inputs_df)),]
  rownames(min_max_df) <- rownames(slider_inputs_df)
  min_max_df$min <- unlist(min_max_df$min)
  min_max_df$max <- unlist(min_max_df$max)
  
  # find the sliders that are not equal to the min max defaults
  diff_values <- anti_join(slider_inputs_df, min_max_df)
  
  # TODO: need to extract which cells are FALSE
  slider_inputs_df == min_max_df

  action <- paste0("The student used filtering with ")
  
} else if (last_page == 'sampling'){
  if (selections$sampling_select_simple_or_stratified == 'Simple'){
    action <- paste0("The student used simple sampling with n = ", sampling_slider_simple_n)
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
