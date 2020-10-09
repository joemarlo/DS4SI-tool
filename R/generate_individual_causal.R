library(tidyverse)
set.seed(44)

load('DS4SI-tool/R/score_causality.RData')
population_dataset <- read_csv('DS4SI-tool/data/jpta.csv')


# This script produces the individual-level data corresponding to
# the hypothetical study where participants were randomized to 
# treatment and control conditions within each site
  
# pull off the causal scores for the sites selected and add to data
dat <- left_join(population_dataset, causal_scores, by = 'Site ID')
n <- nrow(dat) 
 
# expand to size of population
index.pop <- rep(1:n, each = 100) 
data.full <- dat[index.pop,]
  
# generate potential outcomes for each person
# consider the outcome to be income 5 years later
# in thousands of dollars
data.full$y0 <- 10 +
  data.full$Urban * 2 -
  data.full$`Unemployment rate` * 3 -
  data.full$`High school degree rate` * 4 +
  (data.full$`Mean income` / 5) * 3 +
  rnorm(n, 0, 6)
data.full$y1 <- 10 +
  data.full$Urban * 2 -
  data.full$`Unemployment rate` * 3 -
  data.full$`High school degree rate` * 3 +
  (data.full$`Mean income` / 5) * 3 +
  data.full$causal_score  + 
  rnorm(n, 0, 6)

# I included a small easter egg but didn't make it too big as it would
# conflict with the causality index
  
### randomize treatments
# I'm ignoring the group structure when I randomize
data.full$Treatment <- rbinom(n * 100, 1, 0.5) == 1
data.full$Income <- data.full$Treatment * data.full$y1 + (1 - data.full$Treatment) * data.full$y0

# return dataframe with just site id and y0, y1, z, y
individual_causal <- data.full[, c('Site ID', 'Treatment', 'Income')]

save(individual_causal,
     file = 'DS4SI-tool/R/score_causality_individuality.RData')

