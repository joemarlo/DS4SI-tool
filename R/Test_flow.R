library(tidyverse)
theme_set(theme_minimal())

sites.DF <- read.csv("DS4SI-tool/jpta_cleaned.csv")


# joe's assignment code ---------------------------------------------------


#function "scales" the data between 0:1
calc.proportion <- function(distribution){
  (distribution - min(distribution)) / (max(distribution) - min(distribution))
}

C.sites.DF <- sites.DF %>% select(site_id, cost, unemp, pct_hs, income, comfort)
proportions <- apply(X = C.sites.DF, MARGIN = 2, FUN = calc.proportion)
proportions <- proportions[, 2:ncol(proportions)]
colnames(proportions) <- c("cost.P", "unemp.P", "pct_hs.P", "income.P", "comfort.P")
proportions <- as.data.frame(proportions)

#need to make sure percentiles are ordered correctly; high income proportion may mean more income
# cost: down
# unemp: up
# pct_hs: down
# income: down
# comfort: up

#categories to apply the weight against is determined by position in DF
weights <- c(1.25, 1.5, 1, 1.5, 1)

#reverse percentiles for relevant categories; 
#    sum P to generate a single score based on weights
#    bind back to original DF
sites.ranked.DF <- proportions %>%
  mutate(cost.P = 1 - cost.P,
         pct_hs.P = 1 - pct_hs.P,
         income.P = 1 - income.P) %>%
  rowwise() %>%
  # mutate(site_score = sum(c(cost.P, unemp.P, pct_hs.P, income.P, comfort.P))) %>%
  mutate(site_score = sum(c(cost.P, unemp.P, pct_hs.P, income.P, comfort.P) * weights)) %>%
  cbind(sites.DF, .) %>%
  ungroup()

#top sites
top.sites.DF <- sites.ranked.DF %>%
  filter(other_prog == F) %>% #remove sites that have another program
  arrange(desc(site_score)) %>%
  mutate(Exp_site_ct = cummean(comfort)*row_number()) %>%
  filter(Exp_site_ct <= 35) #filters the DF so we expect that we'll have 35 sites after accounting for comfort

#total expected sites
mean(top.sites.DF$comfort) * nrow(top.sites.DF)

#add identifier for top site to the main DF
sites.ranked.DF$top_site <- sites.ranked.DF$site_id %in% top.sites.DF$site_id
rm(C.sites.DF, proportions, top.sites.DF, weights)




#make boxplots which includes weighted sample of sites
#first need to create three groups of data: population,
#  sites to approach, and expected sample of sites (to then apply weights to)
pop.approach.samp <- sites.ranked.DF %>% mutate(Type = "Pop", Weight = 1)
Sites.approach <- pop.approach.samp %>% filter(top_site == TRUE) %>% mutate(Type = "Approach")
Sites.samp <- Sites.approach %>% mutate(Type = "Sample", Weight = comfort)
# Sites.samp$Weight <- calc.proportion(Sites.samp$Weight)
pop.approach.samp <- rbind(pop.approach.samp, Sites.approach, Sites.samp)
#is using comfort the correct way to weight?
p.a.s.plot <- pop.approach.samp %>%
  select(cost, unemp, pct_hs, income, comfort, Type, Weight) %>%
  mutate(Type = factor(Type,
                       levels = c("Sample", "Approach", "Pop"),
                       labels = c("Expected \n sample", "Sites to \n approach", "Population"))) %>%
  rename('Site cost ($)' = cost,
         'County level \n unemployment (%)' = unemp,
         'County level high \n school grad rate (%)' = pct_hs,
         'County level \n median income ($)' = income,
         'Site probability \n of participation' = comfort) %>%
  gather(., key = "key", value = "value", -Type, -Weight) %>%
  ggplot(aes(y = value, x = Type, weight = Weight)) +
  geom_boxplot() +
  stat_summary(fun.y = mean, geom = "errorbar",
               aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") +
  facet_wrap(~key, scale = "free", ncol = 2) +
  labs(x = "", y = "", title = "Distributions of key features")


#plot the distributions: top site vs. total pop
# sites.ranked.DF %>%
#   select(region, urban, other_prog, top_site) %>%
#   gather(., key = "key", value = "value", -top_site) %>%
#   ggplot(aes(x = value)) +
#   # geom_density() +
#   geom_bar(color = "white") +
#   facet_grid(top_site~key, scale = "free_x")


#manually calculate weights
weights <- calc.proportion(Sites.approach$comfort) * 10000

exp.samp <- sapply(c("comfort", "cost", "income", "pct_hs", "unemp"), function(feature){
  wt <- rep(Sites.samp[,feature], times = weights)
  list(mean(wt), median(wt))
}) %>% unlist()

#summary statistics for continuous variables
continuous.summary <- sites.ranked.DF %>%
  filter(top_site == TRUE) %>%
  mutate(top_site = FALSE) %>%
  rbind(sites.ranked.DF) %>% #this duplicates the rows for the "top_site" so the pop boxplots don't exclude the sample
  select(cost, unemp, pct_hs, income, comfort, top_site) %>%
  group_by(top_site) %>%
  summarize_all(list(mean = mean, median = median)) %>%
  select(-top_site) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(rowname) %>%
  rename(Metric = rowname, Population = V1, Sample = V2) %>%
  mutate(Exp.samp = exp.samp)
rm(exp.samp)

#total sites to approach
tlt.app.samp <- sum(sites.ranked.DF$top_site == TRUE)
#total expected sites
tlt.exp.samp <- mean(sites.ranked.DF$comfort[sites.ranked.DF$top_site == TRUE]) * tlt.app.samp
#total cost
total.cost <- sum(sites.ranked.DF$cost[sites.ranked.DF$top_site == TRUE])

#summary stats for categorical variables
cols <- c("region", "urban", "other_prog")
pop <- apply(sites.ranked.DF[, cols], MARGIN = 2, FUN = table) %>% unlist()
sel.samp <- sites.ranked.DF[, cols] %>% filter(sites.ranked.DF$top_site)
aprch <- apply(sel.samp, MARGIN = 2, FUN = table) %>% unlist()
exp.samp <- sapply(cols, function(feature){
  wt <- rep(Sites.samp[,feature], times = weights)
  table(wt) / length(wt) * tlt.exp.samp
}) %>% unlist()
categorical.summary <- bind_rows(pop, aprch, round(exp.samp, 1)) %>%
  t() %>%
  as.data.frame() %>%
  rename(Population = 1, Sample = 2, Exp.samp = 3) %>%
  rownames_to_column("Metric") %>%
  mutate(Pop.per = round(Population / nrow(sites.ranked.DF), 2),
         Samp.per = round(Sample / tlt.app.samp, 2),
         Exp.per = round(Exp.samp / tlt.exp.samp, 2))
rm(cols, pop, sel.samp, aprch, exp.samp)

# summaries ---------------------------------------------------------------
print(paste0("The total number of sites to approach is ", tlt.app.samp)) #total number of sites to approach
print(paste0("The total cost is ", scales::dollar(total.cost))) #total cost
print(paste0("The total number of sites expected to participate is ", round(tlt.exp.samp, 1))) #total number of sites expected to participate
print(continuous.summary)
print(categorical.summary)
print(p.a.s.plot)




