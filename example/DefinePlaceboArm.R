library(dplyr)
library(R6)
library(rlang)
library(survival)


#' define endpoints in arm <pbo>
risk <- data.frame(
  end_time = c(1, 10, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
)

pfs <- Endpoint$new(name = 'pfs', type='tte',
                    generator=PiecewiseConstantExponentialRNG,
                    risk = risk, endpoint_name = 'pfs')

os <- Endpoint$new(name = 'os', type='tte', generator=rexp, rate = log(2)/25)

orr <- Endpoint$new(
  name = 'orr', type = 'binary', generator = rbinom,
  size = 1, prob = .4, readout = c(orr=6))

placebo <- Arm$new(
  name = 'pbo', description = 'Placebo arm')

placebo$add_endpoints(pfs, os, orr)




