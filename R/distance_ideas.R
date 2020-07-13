# library(pdist)
library(pracma)

final_table <- read_csv("DS4SI-tool/jpta_cleaned.csv")


sample_table <- sample_n(final_table, size = 100, replace = FALSE)

numeric_vars <- c("unemp", "pct_hs", "income", "comfort", "cost")
categorical_vars <- c('region', 'urban', 'other_prog')

final_numeric <- final_table[, numeric_vars]
sample_numeric <- sample_table[, numeric_vars]

# define ecdf functions
ecdf_funs <- apply(final_numeric, 2, ecdf)

final_quantiles <- matrix(ncol = length(numeric_vars), nrow = )
sample_quantiles <- final_quantiles
# apply functions
for (i in seq_along(ecdf_funs)){
  final_quantiles[, i] <- ecdf_funs[[i]](final_numeric[[i]])
  sample_quantiles[, i] <- ecdf_funs[[i]](final_numeric[[i]])
}




# -------------------------------------------------------------------------




# calculate quantiles
final_quantiles <- apply(final_numeric, 2, quantile)
sample_quantiles <- apply(sample_numeric, 2, quantile)

apply(final_quantile, 2, sd)

# scale each column in each matrix according to the col in the population dataset


# scale the quantiles so as units as different
  # does this present an issue b/c now the matrices are on different scales
  # from each other
final_scaled <- scale(final_quantiles)
sample_scaled <- scale(sample_quantiles)

# the issue here is that the sample matrix needs to be 'scaled' using the final matrix



# samples -----------------------------------------------------------------

high_unemp_low_hs <- final_table %>% 
  mutate(index = row_number()) %>% 
  filter(unemp > 0.1,
         pct_hs < 0.65)
low_unemp_high_hs <- final_table %>% 
  mutate(index = row_number()) %>% 
  filter(unemp < 0.1,
         pct_hs > 0.75)
mid_unemp_mid_hs <- final_table %>% 
  mutate(index = row_number()) %>% 
  filter(unemp > 0.07,
         unemp < 0.13,
         pct_hs < 0.75,
         pct_hs > 0.65)


# idea 1 ------------------------------------------------------------------

calc_dist <- function(sample_mat, population_mat) {
  
  # step 1: reduce matrices to the same dimensions using quantile
  # step 2: scale the matrices so the column units do not affect the final score
    # make sure both matrices are scaled the same way
  # step 3: calculate distance between the respective columns b/t the matrices
  # step 4: sum the distances into a single metric
  
  # calculate quantiles
  pop_quantiles <- apply(population_mat, 2, quantile, probs = seq(0, 1, 0.1))
  sample_quantiles <- apply(sample_mat, 2, quantile, probs = seq(0, 1, 0.1))

  # calculate the distance between column[i] of matrix 1 vs column[i] of matrix 2
  col_distances <- c()
  for (i in 1:ncol(population_mat)) {
    
    # calculate mean and sd of the column in the population matrix
    mean_sd <- mean(sd(pop_quantiles[, i]))
    pop_sd <- sd(pop_quantiles[, i])
    
    # scale each matrix according to pop mean and sd
    pop_scaled <- (pop_quantiles[, i] - mean_sd) / pop_sd
    sample_scaled <- (sample_quantiles[, i] - mean_sd) / pop_sd
    
    # calculate distance between the columns
    col_distances[i] <- distmat(sample_scaled, pop_scaled)
  }
  
  # collapse to a single metric by summing
  mat_distance <- sum(col_distances)
  
  return(mat_distance)
}


numeric_vars <- c("unemp", "pct_hs", "income", "comfort", "cost")
categorical_vars <- c('region', 'urban', 'other_prog')

final_numeric <- final_table[, numeric_vars]
sample_numeric <- sample_table[, numeric_vars]

calc_dist(sample_numeric, final_numeric)

rslts <- sapply(1:10000, function(i){
  
  sample_numeric <- sample_n(final_numeric, size = runif(1, min = 10, max = 350), replace = FALSE)
  calc_dist(sample_numeric, final_numeric)
  
})



# idea 2 ------------------------------------------------------------------

# PCA the population
# then color the sample
# then figure out from here distance between the sample and the population center

# step 1: PCA the population
# step 2: calculate means of each sample PC1:PCn data point
# step 3: calculate euclidean distance between it and 0 (population mean)
# it seams like taking the mean removes the spread of the distribution
# maybe take the distance of euclidean dist of all the datapoints

sample_indices <- sample(1:nrow(final_numeric), size = 100, replace = FALSE)

pca <- princomp(scale(final_numeric))

plot(pca$scores[,1], pca$scores[,2])
points(pca$scores[sample_indices,1], pca$scores[sample_indices,2], col = 'red')
points(mean(pca$scores[,1]), mean(pca$scores[,2]), pch = 8, cex = 2, lwd = 2)

pca$scores %>% 
  as_tibble() %>%
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = 0.1) +
  geom_point(data = tibble(x = mean(pca$scores[,1]),
                           y = mean(pca$scores[,2])),
             aes(x=x, y=y), color = 'black', size = 3) +
  geom_point(data = tibble(x = mean(pca$scores[sample_indices,1]),
                           y = mean(pca$scores[sample_indices,2])),
             aes(x=x, y=y), color = 'blue', size = 3) +
  geom_point(data = tibble(x = pca$scores[sample_indices,1],
                           y = pca$scores[sample_indices,2]),
             aes(x=x, y=y), color = 'blue', alpha = 0.3)
             

sample_means <- map_dfr(1:n_sim, function(i){
  
  ind <- sample(1:nrow(final_numeric), size = 100, replace = FALSE)
  
  tibble(Comp.1 = mean(pca$scores[ind, 1]),
         Comp.2 = mean(pca$scores[ind, 2]),
       sim = i)
})


pca$scores %>% 
  as_tibble() %>%
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = 0.1) +
  geom_point(data = tibble(x = mean(pca$scores[,1]),
                           y = mean(pca$scores[,2])),
             aes(x=x, y=y), color = 'red', size = 3) +
  geom_point(data = sample_means, color = 'blue', alpha = 0.1)


pca$scores %>% 
  as_tibble() %>%
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = 0.1) +
  geom_point(data = tibble(x = mean(pca$scores[,1]),
                           y = mean(pca$scores[,2])),
             aes(x=x, y=y), color = 'black', size = 3) +
  geom_point(data = tibble(x = mean(pca$scores[high_unemp_low_hs$index,1]),
                           y = mean(pca$scores[high_unemp_low_hs$index,2])),
             aes(x=x, y=y), color = 'blue', size = 3) +
  geom_point(data = tibble(x = pca$scores[high_unemp_low_hs$index,1],
                           y = pca$scores[high_unemp_low_hs$index,2]),
             aes(x=x, y=y), color = 'blue', alpha = 0.3)

pca$scores %>% 
  as_tibble() %>%
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = 0.1) +
  geom_point(data = tibble(x = mean(pca$scores[,1]),
                           y = mean(pca$scores[,2])),
             aes(x=x, y=y), color = 'black', size = 3) +
  geom_point(data = tibble(x = mean(pca$scores[low_unemp_high_hs$index,1]),
                           y = mean(pca$scores[low_unemp_high_hs$index,2])),
             aes(x=x, y=y), color = 'blue', size = 3) +
  geom_point(data = tibble(x = pca$scores[low_unemp_high_hs$index,1],
                           y = pca$scores[low_unemp_high_hs$index,2]),
             aes(x=x, y=y), color = 'blue', alpha = 0.3)


pca$scores %>% 
  as_tibble() %>%
  ggplot(aes(x = Comp.1, y = Comp.2)) +
  geom_point(alpha = 0.1) +
  geom_point(data = tibble(x = mean(pca$scores[,1]),
                           y = mean(pca$scores[,2])),
             aes(x=x, y=y), color = 'black', size = 3) +
  geom_point(data = tibble(x = mean(pca$scores[mid_unemp_mid_hs$index,1]),
                           y = mean(pca$scores[mid_unemp_mid_hs$index,2])),
             aes(x=x, y=y), color = 'blue', size = 3) +
  geom_point(data = tibble(x = pca$scores[mid_unemp_mid_hs$index,1],
                           y = pca$scores[mid_unemp_mid_hs$index,2]),
             aes(x=x, y=y), color = 'blue', alpha = 0.3)


# idea 3 ------------------------------------------------------------------

# calclate eucl distance of the entire pca matrix
# issue here is that the sample size will correlate with the distance b/c
  # fewer datapoints = less sum distance

sqrt(sum(pca$scores^2))

calc_dist_2 <- function(sample_mat, pca = pca){
  
  # calculate the mean of each PC space for just the sample pooints
  means <- apply(pca$scores[sample_mat$index, ], 2, mean)
  
  # euclidean distance of means of each PC comp
  sqrt(sum(means^2))
  
}

# testing -----------------------------------------------------------------

n_sim <- 10000
sizes <- runif(n_sim, min = 10, max = 350)

final_numeric_ind <- final_numeric
final_numeric_ind$index <- 1:nrow(final_numeric_ind)

samples <- lapply(sizes, function(n) sample_n(final_numeric_ind, size = n, replace = FALSE))
samples_wo_ind <- lapply(samples, function(tbl) tbl[, 1:5])
idea_1_results <- sapply(samples, function(samp) calc_dist(samp, final_numeric))

idea_2_results <- sapply(samples, function(samp){
  
  # calculate the mean of each PC space for just the sample pooints
  means <- apply(pca$scores[samp$index, ], 2, mean)
  
  # euclidean distance of means of each PC comp
  sqrt(sum(means^2))
  })

idea_3_results <- sapply(samples, function(samp){
  sqrt(sum(pca$scores[samp$index,]^2))
})
# subtract population score
idea_3_results <- idea_3_results - sqrt(sum(pca$scores^2))

plot(density(idea_3_results))

plot(idea_1_results, idea_2_results)
plot(rank(idea_1_results), rank(idea_2_results))


(rank(idea_1_results) - rank(idea_2_results)) %>% density() %>% plot()


plot(ecdf(idea_2_results)(idea_2_results), ecdf(idea_1_results)(idea_1_results))
ecdf(idea_2_results)(idea_2_results)


calc_dist(high_unemp_low_hs[, numeric_vars], final_numeric)
calc_dist(low_unemp_high_hs[ numeric_vars], final_numeric)
calc_dist(mid_unemp_mid_hs[ numeric_vars], final_numeric)

calc_dist_2(high_unemp_low_hs[, c(numeric_vars, 'index')], pca = pca)
calc_dist_2(low_unemp_high_hs[ c(numeric_vars, 'index')],  pca = pca)
calc_dist_2(mid_unemp_mid_hs[ c(numeric_vars, 'index')],  pca = pca)


