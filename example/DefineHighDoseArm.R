#' hazard ratios
hr <- c(pfs_low = 0.7, os_low = 0.8, pfs_high = 0.65, os_high = 0.75)

#' define endpoints in the high dose arm
pfs <- Endpoint$new(name = 'pfs', type='tte',
                    generator = rexp, rate = log(2)/5.45 * hr['pfs_high'])

vrr <- Endpoint$new(
  name = 'vrr', type = 'binary', readout = c(vrr = 6 / 52 * 12),
  generator = rbinom, size = 1, prob = .25) # higher probability

os <- Endpoint$new(name = 'os', type='tte',
                   generator=rexp, rate = log(2)/13.71 * hr['os_high'])

high <- Arm$new(name = 'high dose', description = 'treated with high dose')
high$add_endpoints(pfs, os, vrr)
