knitr::opts_chunk$set(
  collapse = TRUE,
  cache.path = 'cache/crossoverWashout/',
  comment = '#>',
  dpi = 300,
  out.width = '100%'
)

library(TrialSimulator)
library(mvtnorm)
library(dplyr)
library(kableExtra)


rng <- function(n, means, vcov = diag(1, 8)){
  ret <- as.data.frame(rmvnorm(n, mean = means, sigma = vcov))
  colnames(ret) <- c('baseline1', 'ep1', 
                     'baseline2', 'ep2', 
                     'baseline3', 'ep3', 
                     'baseline4', 'ep4')
  ret
}

all_endpoint_name <- c('baseline1', 'ep1', 
                       'baseline2', 'ep2', 
                       'baseline3', 'ep3', 
                       'baseline4', 'ep4')

readouts <- c(baseline1 = 0, ep1 = 3, 
              baseline2 = 4, ep2 = 8, 
              baseline3 = 10, ep3 = 15, 
              baseline4 = 17.5, ep4 = 19.5)

eps <- endpoint(
  name = all_endpoint_name,
  type = rep('non-tte', 8), 
  readout = readouts, 
  generator = rng, means = rep(c(0, .5), 4)
)

arm1 <- arm(name = 'ABCD')
arm1$add_endpoints(eps)
arm1

# NA

eps <- endpoint(
  name = all_endpoint_name,
  type = rep('non-tte', 8), 
  readout = readouts, 
  generator = rng, means = rep(c(0, .6), 4) # diff means
)

arm2 <- arm(name = 'BDAC')
arm2$add_endpoints(eps)

eps <- endpoint(
  name = all_endpoint_name,
  type = rep('non-tte', 8), 
  readout = readouts, 
  generator = rng, means = rep(c(0, .2), 4), vcov = diag(1.2, 8) # diff means/vcov
)

arm3 <- arm(name = 'CADB')
arm3$add_endpoints(eps)

eps <- endpoint(
  name = all_endpoint_name,
  type = rep('non-tte', 8), 
  readout = readouts, 
  generator = rng, means = rep(c(0, 0), 4) # diff means
)

arm4 <- arm(name = 'DCBA')
arm4$add_endpoints(eps)

accrual_rate <- data.frame(end_time = c(6, Inf),
                           piecewise_rate = c(10, 10))

trial <- trial(seed = 1213L, name = 'crossover-trial', 
               n_patients = 60, 
               duration = 28,
               enroller = StaggeredRecruiter, accrual_rate = accrual_rate, 
               silent = TRUE)

trial$add_arms(sample_ratio = c(1, 1, 1, 1), arm1, arm2, arm3, arm4)
trial

action <- function(trial){
  locked_data <- trial$get_locked_data('final')
  ## omit statistical analysis
  
  trial$save(value = 'anything', name = 'result')
  ## save more results for summary of simulation
  # trial$save(value = ..., name = ...)
}

final <- milestone(name = 'final', 
                   when = calendarTime(time = 25.5), 
                   action = action)

final <- milestone(name = 'final', 
                   when = eventNumber(endpoint = 'ep4', n = 60), 
                   action = action)

final <- milestone(name = 'final', 
                   when = enrollment(n = 60, min_treatment_duration = 19.5), 
                   action = action)

# NA

listener <- listener()
listener$add_milestones(final)

controller <- controller(trial, listener)
