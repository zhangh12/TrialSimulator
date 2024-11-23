
risk <- data.frame(
  end_time = c(1, 10, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01),
  hazard_ratio = .9
)

pfs <- Endpoint$new(name = 'pfs', type='tte', method='piecewise_const_exp',
                     risk = risk)
orr <- Endpoint$new(
  name = 'orr', type = 'binary', generator = rbinom,
  size = 1, prob = .45)

os <- Endpoint$new(name = 'os', type='tte', rexp, rate = log(2)/25 * .85)

low <- Arm$new(
  name = 'low dose', description = 'Low dose arm')
low$add_endpoints(pfs, os, orr)

