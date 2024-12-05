
## Plan a trial, Trial-3415, of up to 100 patients.
## Enrollment time follows an exponential distribution, with median 5


trial <- Trial$new(
  name = 'Trial-3415', n_patients = n_patients,
  seed = 31415926,
  enroller = rexp, rate = log(2) / 5)

trial$add_arms(sample_ratio = 1, placebo)
trial$add_arms(sample_ratio = c(1, 1), enforce = TRUE, low, high)
