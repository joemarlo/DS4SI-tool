library(tidyverse)
library(caret) # for dummy coding
library(parallel) # for building simulated distribution for score_generalizability()
set.seed(44)

population_dataset <- read_csv('DS4SI-tool/data/jpta.csv')
numeric_vars <- sort(c("Unemployment rate", "High school degree rate", "Mean income", 
                       "Comfort", "Cost to approach site", "Cost to run study"))
population_dataset <- population_dataset[, setdiff(colnames(population_dataset), "Other program at site")]

# dummy code the variables
dummies <- dummyVars(~ Urban + Region, population_dataset) %>% predict(., population_dataset)
dummied_data <- cbind(dummies, population_dataset[, numeric_vars])

# run PCA
population_pca <- princomp(scale(dummied_data))

calc_generalizability <- function(sample_data, pca = population_pca, population = population_dataset){
  # function returns a generalizability metric based on PCA
  # it works by subseting the pca object to only contain the observations in the sample
    # and then returns the mean score (linear combinations) of the sample. This score would
    # equal 0 for the population therefore the absolute value of the sample score is it's
    # 'distance' from the population
  
  # get the row indices of the sample rows within the population dataset
  population$index <- 1:nrow(population)
  indices <- population[population$`Site ID` %in% sample_data$`Site ID`,]$index
  
  # for the sample_data, calculate euclidean distance of each row
  # from the center of (which = 0)
  # site_euclidean_distance <- apply(pca$scores[indices,], 1, function(row){
  #   sqrt(sum(row^2))
  # })
  
  # take the overall mean of the distances for that sample
  # generalizability_score <- mean(site_euclidean_distance)
  
  # slice the population_pca using the indices then calculate the absolute value 
  # of the mean of the scores
  generalizability_score <- abs(mean(pca$scores[indices,]))
  
  return(generalizability_score)
}

# to normalize the score between [0,1], create a simulated distribution
  # of scores based on random samples of the population
# keep in mind the scores are correlated with sample size
nsims <- 5000
ns <- sample(100:1000, size = nsims, replace = TRUE)
raw_scores <- mclapply(ns, function(n){
  boolean_urban <- sample(list(TRUE, FALSE, c(TRUE, FALSE)), size = 1) %>% unlist()
  regions <- sample(c("South", "Northeast", "South", "West"), size = runif(1, min = 1, max = 4))
  boolean <- (population_dataset$Urban %in% boolean_urban) &
    (population_dataset$Region %in% regions)
  my_sample <- slice_sample(population_dataset[boolean,], 
                            n = n, replace = FALSE)
  # my_sample <- slice_sample(population_dataset, n = ns[i])
  calc_generalizability(my_sample)
} ) %>% unlist()

# hist(raw_scores, breaks = seq(0, 1, 0.01))
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

# this should be ~1
score_generalizability(population_dataset)

# see how the scores change with sample size
ns <- rep(seq(100, 1000, by = 50), 200)
x <- sapply(ns, function(n){
  boolean_urban <- sample(list(TRUE, FALSE, c(TRUE, FALSE)), size = 1) %>% unlist()
  regions <- sample(c("South", "Northeast", "South", "West"), size = runif(1, min = 1, max = 4))
  boolean <- (population_dataset$Urban %in% boolean_urban) &
    (population_dataset$Region %in% regions)
  my_sample <- slice_sample(population_dataset[boolean,], 
                            n = n, replace = FALSE)
  score_generalizability(my_sample)
})
x %>% 
  enframe() %>% 
  mutate(group = ns) %>% 
  ggplot(aes(x = value, group = group)) + 
  geom_density() + 
  facet_wrap(~group, scales = 'free_y')
rm(x)


save(calc_generalizability, score_generalizability, ecdf_scores, population_pca,
     file = 'DS4SI-tool/R/score_generalizability.RData')

