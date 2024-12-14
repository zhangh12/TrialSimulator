library(dplyr)
library(R6)
library(rlang)
library(survival)


#' define endpoints in arm <pbo>
pfs <- Endpoint$new(name = 'pfs', type='tte',
                    generator=rexp, rate = log(2) / 5.45)
os <- Endpoint$new(name = 'os', type='tte',
                   generator = rexp, rate = log(2)/13.71)
#' define binary endpoint VRR, with readout time = 6 weeks
vrr <- Endpoint$new(
  name = 'vrr', type = 'binary', readout = c(vrr=6 /52 * 12),
  generator = rbinom, size = 1, prob = .01)
#' define the placebo arm
placebo <- Arm$new(name = 'pbo', description = 'placebo')
#' add endpoints into the arm
placebo$add_endpoints(pfs, os, vrr)
