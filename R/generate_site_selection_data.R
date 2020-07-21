# Code to create fake data for simulation

# Pretend that you have been charged with evaluating the impact of
# a job training program.  The target population is similar to the
# population targeted by the JTPA.  However the hypothetical program
# is more fully defined with a prescribed curriculum, activities,
# and requirements for participation.

# Your goal is to use these data to choose the list of sites that you
# will try to recruit to be part of your study.  The challenge is to
# balance the goals of the evaluators (and funders) with the probability
# that a site will actually agree to participate.  The evaluators goals
# could be summarized

# The datasets available to you has the following information on 
# N potential sites (local AFDC offices)
N = 1000
set.seed(1234)

# site ID
site_id = sample(seq(1000,9999,1), N, replace=FALSE)

# region of country and urban/rural
region = sample(c(1,2,3,4), N, replace=TRUE, prob=c(.3,.2,.3,.3))
# 1 = n. east, 2 = n. central, 3= south, 4= west
urban = rbinom(N,1,.2)

# travel time in hours between the site and the nearest MDRC office
distance = rnorm(N,2-urban+(region==3),.5)

# other training programs available in the same county
other_prog = rbinom(N,1,prob=rep(.2,N)+urban*.2)

# unemployment rate in the county that the site is in
unemp = runif(N,.05,.15)

# percent of adults with high school degrees
pct_hs = rnorm(N,.8-unemp,.05)

# the average income (in thousands of $) in the county that the site is in
# (really should be logged)
income_hat = 40+20*pct_hs+5*urban+10*(region!=3)
income_hat = (1-unemp)*income_hat + unemp*0 
income = income_hat + rexp(N,.5)

# program admin response to survey about comfort level with random assn
# the idea is that there should be a relationship between comfort level 
# the probability that they agree to participate
# moreover there should be a trade-off between increasing this probability 
# and getting a sample that will generalize to the full population
comfort = rnorm(N,.5+.4*pct_hs-other_prog*.1-urban*.1)
comfort = (comfort + abs(min(comfort))+.6)
comfort = comfort/(max(comfort)+.4)

# pull into one dataset
data = data.frame(site_id=site_id, region=region,urban=urban,
                  distance=distance,other_prog=other_prog,unemp=unemp,
                  pct_hs=pct_hs,income=income,comfort=comfort)


# # 2020 cleaning ---------------------------------------------------------

# label regions
data$region <- case_when(
  data$region == 1 ~ "Northeast",
  data$region == 2 ~ "Northcentral",
  data$region == 3 ~ "South",
  data$region == 4 ~ "West"
)

# convert booleans to logicals
data$urban <- as.logical(data$urban)
data$other_prog <- as.logical(data$other_prog)

# convert distance to cost
data$cost <- round(1000 + (data$distance * 500), 0)
data <- data[, setdiff(colnames(data), "distance")]

# round the numerics to three digits
data[, c("unemp", "pct_hs", "income", "comfort")] <-
  round(data[, c("unemp", "pct_hs", "income", "comfort")] , 3)


# save the data -----------------------------------------------------------

# save the data
write.table(
  data,
  file = "DS4SI-tool/data/jpta.csv",
  col.names = TRUE,
  row.names = FALSE,
  sep = ","
)
#write.table(data, file="jpta.csv",col.names=TRUE,sep=",")
