library(tidyverse)
library(caret) # for dummy coding
library(parallel) # for building simulated distribution for score_generalizability()

# dummy code the variables
dummies <- dummyVars(~ urban + other_prog + region, population_dataset) %>% predict(., population_dataset)
dummied_data <- cbind(dummies, population_dataset[, numeric_vars])

# run PCA
population_pca <- princomp(scale(dummied_data))

calc_generalizability <- function(sample_data, pca = population_pca, population = population_dataset){
  
  # returns a generalizability metric based on PCA
  
  # get row indices
  indices <- suppressMessages(
    population %>%
      mutate(index = row_number()) %>%
      semi_join(sample_data) %>%
      pull(index)
  )
  
  # for the sample_data, calculate euclidean distance of each row
  # from the center of (which = 0)
  # site_euclidean_distance <- apply(pca$scores[indices,], 1, function(row){
  #   sqrt(sum(row^2))
  # })
  
  # take the overall mean of the distances for that sample
  # generalizability_score <- mean(site_euclidean_distance)
  
  # calculate mean of the absolute value of the scores for the sample
  generalizability_score <- abs(mean(pca$scores[indices,]))
  
  return(generalizability_score)
}


# create an simulated distribution of generalizability metrics
nsims <- 10000
ns <- sample(2:nrow(population_dataset), size = nsims, replace = TRUE)
raw_scores <- mclapply(1:nsims, function(i){
  my_sample <- slice_sample(population_dataset, n = ns[i], replace = FALSE)
  calc_generalizability(my_sample)
} ) %>% unlist()

# save the ecdf
ecdf_scores <- ecdf(raw_scores)

score_generalizability <- function(...){
  # function is a wrapper around calc_generalizability
  # takes the output and compares it to a pre-calculated
  # distribution of raw scores
  
  raw_score <- calc_generalizability(...)
  score <- 1 - ecdf_scores(raw_score)
  return(score)
}

score_generalizability(population_dataset)

