knitr::opts_chunk$set(
  collapse = TRUE,
  cache.path = 'cache/responseAdaptive/',
  comment = '#>',
  dpi = 300,
  out.width = '100%'
)

library(dplyr)
library(kableExtra)
library(DoseFinding)
library(TrialSimulator)
set.seed(12345)

rng <- function(n, dose){

  model <- DoseFinding::Mods(
    emax = c(2.6, 12.5),
    placEff = 1.25, maxEff = 0.1,
    doses = c(0, 20, 25, 50, 100))

  data.frame(
    fev1 = rnorm(n, mean = DoseFinding::getResp(model, doses = dose), sd = .05)
  )

}

fev1 <- endpoint(name = 'fev1', type = 'non-tte', readout = c(fev1 = 4),
                 generator = rng, dose = 0)
pbo <- arm(name = '0.0')
pbo$add_endpoints(fev1)

fev1 <- endpoint(name = 'fev1', type = 'non-tte', readout = c(fev1 = 4),
                 generator = rng, dose = 20.0)
dose1 <- arm(name = '20.0')
dose1$add_endpoints(fev1)

fev1 <- endpoint(name = 'fev1', type = 'non-tte', readout = c(fev1 = 4),
                 generator = rng, dose = 25.0)
dose2 <- arm(name = '25.0')
dose2$add_endpoints(fev1)

fev1 <- endpoint(name = 'fev1', type = 'non-tte', readout = c(fev1 = 4),
                 generator = rng, dose = 30.0)
dose3 <- arm(name = '30.0')
dose3$add_endpoints(fev1)

fev1 <- endpoint(name = 'fev1', type = 'non-tte', readout = c(fev1 = 4),
                 generator = rng, dose = 35.0)
dose4 <- arm(name = '35.0')
dose4$add_endpoints(fev1)

accrual_rate <- data.frame(end_time = c(24, Inf),
                           piecewise_rate = c(100/24, 100/12))
trial <- trial(
  name = 'Trial-3415', n_patients = 200,
  seed = 1727811904, duration = 40,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  silent = TRUE
)

trial$add_arms(sample_ratio = rep(1, 5), pbo, dose1, dose2, dose3, dose4)
trial

compute_sample_ratio <- function(data){

  data$dose <- as.numeric(data$arm)
  fit <- lm(fev1 ~ factor(dose) - 1, data = data)
  dose <- unique(sort(data$dose))
  mu_hat <- coef(fit)
  S_hat <- vcov(fit)

  suppressMessages(
    ma_fit <- DoseFinding::maFitMod(dose, mu_hat, S = S_hat,
                                    models = c("emax", "sigEmax", "quadratic"))
  )

  pred <- predict(ma_fit, doseSeq = c(0, 20, 25, 30, 35), summaryFct = NULL)
  prob <- apply(pred[, -1] - pred[, 1], 2, function(x){mean(x > .08)})
  sample_ratio <- c(.2, (1 - .2) * prob / sum(prob)) %>% unname()

  sample_ratio
}

multiple_contrast_test <- function(data){
  
  candidate_models <- DoseFinding::Mods(
    emax = c(2.6, 12.5), sigEmax = c(30.5, 3.5), quadratic = -0.00776,
    placEff = 1.25, maxEff = 0.15, doses = c(0, 20, 25, 30, 35))

  data$dose <- as.numeric(data$arm)
  test <- DoseFinding::MCTtest(dose = dose, resp = fev1,
                               models = candidate_models, data = data)
  
  ## at least one dose shows significant non-flatten pattern
  any(attr(test$tStat, 'pVal') < .05)

}

stage_action <- function(trial, milestone_name){

  locked_data <- trial$get_locked_data(milestone_name)
  new_sample_ratio <- compute_sample_ratio(locked_data)

  trial$update_sample_ratio(arm_names = c('0.0', '20.0', '25.0', '30.0', '35.0'),
                            sample_ratios = new_sample_ratio)
  
  message(milestone_name, ': ')
  data.frame(table(locked_data$arm), new_sample_ratio) %>%
    setNames(c('dose', 'total_n', 'new_ratio')) %>% print()

}

final_action <- function(trial){

  locked_data <- trial$get_locked_data('final')
  
  message('final: ')
  data.frame(table(locked_data$arm)) %>%
    setNames(c('dose', 'total_n')) %>% print()

  trial$save(value = multiple_contrast_test(locked_data),
             name = 'MC_test')
}

stage1 <- milestone(name = 'stage 1',
                    when = eventNumber('fev1', n = 50),
                    action = stage_action, milestone_name = 'stage 1')

stage2 <- milestone(name = 'stage 2',
                    when = eventNumber('fev1', n = 120),
                    action = stage_action, milestone_name = 'stage 2')

final <- milestone(name = 'final',
                   when = eventNumber('fev1', n = 200),
                   action = final_action)

# NA

# NA

listener <- listener()
listener$add_milestones(stage1, stage2, final)

controller <- controller(trial, listener)
