library(tidyverse)
set.seed(44)

population_dataset <- read_csv('DS4SI-tool/data/jpta.csv')

# create df of just site_id
causal_scores <- population_dataset[, 'Site ID']

# add causal score to the df
# score is a 4 for sites without other_prog and 1 with
# add rnorm noise with sd = 0.35
# make sure score is not below 0
causal_scores$causal_score <- pmax(0, rnorm(
  n = nrow(causal_scores),
  mean = ((!population_dataset$`Other program at site`) * 3) + 1,
  sd = 0.35
))

score_causality <- function(data, scores_df = causal_scores){
  # function returns a single numeric between [0, 1]
  # that represents the causal score
  # 1 indicates a perfect score
  
  # get the causal scores per sites that are selected
  selected_sites <- left_join(data, scores_df, by = 'Site ID')
  
  # define the best possible and worst possible scores as the means of the 
  # top 100 and bottom 100 site scores
  # best_sites <- scores_df[order(-scores_df$causal_score),][1:min_sites_to_approach,]
  # best_score <- mean(best_sites$causal_score)
  # worst_sites <- scores_df[order(scores_df$causal_score),][1:min_sites_to_approach,]
  # worst_score <- mean(worst_sites$causal_score)
  
  # define the user's score by taking the average score and then
  # scaling it between the top and worst scores
  score <- mean(selected_sites$causal_score)
  # scaled_score <- (score - worst_score) / (best_score - worst_score)
  
  return(score)
}

# testing
# scores <- sapply(rep(100, 5000), function(i){
#   boolean <- sample(list(TRUE, FALSE, c(TRUE, FALSE)), size = 1) %>% unlist()
#   slice_sample(population_dataset[population_dataset$other_prog %in% boolean,], n = i) %>% score_causality()
# })
# 
# summary(scores)
# plot(density(scores))

save(score_causality, causal_scores,
     file = 'DS4SI-tool/R/score_causality.RData')
