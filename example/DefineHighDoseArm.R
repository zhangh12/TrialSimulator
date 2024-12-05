
risk <- data.frame(
  end_time = c(1, 10, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01),
  hazard_ratio = .7
)

pfs <- Endpoint$new(name = 'pfs', type='tte',
                    generator=PiecewiseConstantExponentialRNG,
                    risk = risk,
                    endpoint_name = 'pfs')
orr <- Endpoint$new(
  name = 'orr', type = 'binary', generator = rbinom,
  size = 1, prob = .55, readout = c(orr=6))

os <- Endpoint$new(name = 'os', type='tte', generator=rexp, rate = log(2)/25 * .75)

high <- Arm$new(
  name = 'high dose', description = 'High dose arm')
high$add_endpoints(pfs, os, orr)

