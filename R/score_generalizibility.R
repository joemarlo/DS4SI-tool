library(tidyverse)
library(caret) # for dummy coding
library(parallel) # for building simulated distribution for score_generalizability()
set.seed(44)

population_dataset <- read_csv('data/jpta.csv')
numeric_vars <- sort(c("unemp", "pct_hs", "income", "comfort", "cost"))

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


# to normalize the score between [0,1], create a simulated distribution
  # of scores based on random samples of the population
# the scores are correlated with sample size so cap the sample size at 500
  # otherwise the scores the students receieve will be quite low as their 
  # their final samples should be around 100
nsims <- 1000
ns <- sample(50:500, size = nsims, replace = TRUE)
raw_scores <- mclapply(1:nsims, function(i){
  my_sample <- slice_sample(population_dataset, n = ns[i], replace = FALSE)
  calc_generalizability(my_sample)
} ) %>% unlist()

# plot(density(raw_scores))

# save the ecdf
ecdf_scores <- ecdf(raw_scores)

score_generalizability <- function(...){
  # function is a wrapper around calc_generalizability
  # takes the output and compares it to a pre-simulated
    # distribution of raw scores
  
  raw_score <- calc_generalizability(...)
  score <- 1 - ecdf_scores(raw_score)
  return(score)
}

# this should equal 1
score_generalizability(population_dataset)

# see how the scores change with sample size
x <- sapply(rep(seq(100, 1000, by = 50), 100), function(x) score_generalizability(slice_sample(population_dataset, n = x)))
x %>% 
  enframe() %>% 
  mutate(group = rep(seq(100, 1000, by = 50), 100)) %>% 
         # group = cut(group, breaks = seq(100, 100, by = 25))) %>% 
  ggplot(aes(x = value, group = group)) + 
  geom_density() + 
  facet_wrap(~group, scales = 'free_y')
rm(x)


save(calc_generalizability, score_generalizability, ecdf_scores, population_pca,
     file = 'R/score_generalizability.RData')

