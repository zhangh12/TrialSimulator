#' define endpoints in the low dose arm
#' hazard ratios
hr <- c(pfs_low = 0.7, os_low = 0.8, pfs_high = 0.65, os_high = 0.75)

pfs <- Endpoint$new(name = 'pfs', type='tte',
                    generator = rexp, rate = log(2)/5.45 * hr['pfs_low'])

vrr <- Endpoint$new(
  name = 'vrr', type = 'binary', readout = c(vrr=6 / 52 * 12),
  generator = rbinom, size = 1, prob = .15) # lower probability

os <- Endpoint$new(name = 'os', type='tte',
                   generator=rexp, rate = log(2)/13.71 * hr['os_low'])

low <- Arm$new(name = 'low dose', description = 'treated with low dose')
low$add_endpoints(pfs, os, vrr)
