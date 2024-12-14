
## Plan a trial, Trial-3415, of up to 100 patients.

accrual_rate <- data.frame(end_time = c(14, Inf), piecewise_rate = c(15, 20))
trial <- Trial$new(
  name = 'Trial-3415', n_patients = 750,
  seed = 31415, duration = 64,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate)

trial$add_arms(sample_ratio = 1, placebo)
trial$add_arms(sample_ratio = c(1, 1), enforce = TRUE, low, high)
