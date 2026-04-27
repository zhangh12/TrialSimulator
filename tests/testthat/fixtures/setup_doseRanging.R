knitr::opts_chunk$set(
  collapse = TRUE,
  cache.path = 'cache/doseRanging/',
  comment = '#>',
  dpi = 300,
  out.width = '100%'
)

library(TrialSimulator)
library(DoseFinding)
library(dplyr)
library(kableExtra)
set.seed(12345)

mods <- DoseFinding::Mods(sigEmax = rbind(c(1, 3)), 
                          placEff = log(.1/(1 - .1)), 
                          maxEff = log(.25/(1 - .25)) - log(.1/(1 - .1)),
                          doses = c(0, 0.5, 1.5, 2.5, 4))

DoseFinding::plotMods(mods, trafo = function(x) 1/(1+exp(-x)))

## response rates on curve
x <- DoseFinding::getResp(mods, doses = c(0, 0.5, 1.5, 2.5, 4))
1 / (1 + exp(-unclass(x)))

ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 1),
               generator = rbinom, size = 1, prob = 0.1)
pbo <- arm(name = 'dose = 0.0')
pbo$add_endpoints(ep)

ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 1),
               generator = rbinom, size = 1, prob = 0.25)
trt4 <- arm(name = 'dose = 4.0')
trt4$add_endpoints(ep)

accrual_rate <- data.frame(end_time = c(7, Inf),
                           piecewise_rate = c(5, 20))

trial <- trial(seed = 789L, name = '123', n_patients = 150, duration = 14,
               enroller = StaggeredRecruiter, accrual_rate = accrual_rate, 
               silent = TRUE)

trial$add_arms(sample_ratio = c(1, 1), pbo, trt4)
trial

go_nogo <- function(data){

  ## candidate models for MCPMod
  doses <- c(0, 0.5, 1.5, 2.5, 4)
  candidates <- Mods(emax = c(.25, 1),
                     sigEmax = rbind(c(1, 3), c(2.5, 4)),
                     betaMod = c(1.1, 1.1),
                     placEff = log(.1/(1 - .1)),
                     maxEff = log(.25/(1 - .25)) - log(.1/(1 - .1)),
                     doses = doses)

  fit <- glm(ep ~ factor(arm) + 0, data = data, family = binomial)
  mu_hat <- coef(fit)
  S_hat <- vcov(fit)

  ## multiple contrast test
  test <- DoseFinding::MCTtest(dose = doses,
                               mu_hat, S = S_hat,
                               models = candidates, type = "general")

  ## model averaging
  model <- DoseFinding::maFitMod(dose = doses,
                                 mu_hat, S = S_hat,
                                 models = c("emax", "sigEmax", "betaMod"))
  
  ## predict response rate per dose
  prd <- predict(model, summaryFct = median, doseSeq = doses)
  
  ## convert to scale of probability
  prd_rate <- 1 / (1 + exp(-prd))

  ## go/no-go rule: MCP test p-value < 0.05 and estimated effect > 10%
  ifelse(min(attr(test$tStat, 'pVal')) < .05 &
           max(prd_rate - prd_rate[1]) > .1, 'go', 'no-go')

}

action_at_interim <- function(trial){

  ## get data snapshot
  locked_data <- trial$get_locked_data('interim')

  ## compare two arms
  ## Risk difference = response rate in high dose - response rate in placebo
  fit <- fitLogistic(ep ~ arm, placebo = 'dose = 0.0',
                     data = locked_data, alternative = 'greater',
                     scale = 'risk difference')

  ## for summary of early termination
  trial$save(value = fit$z, name = 'z_value')
  trial$save(value = ifelse(fit$z > 1.64, 'add dose arms', 'stop trial'), 
             name = 'interim_decision')

  ## create three dose arms
  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 1),
                 generator = rbinom, size = 1, prob = .112)
  trt1 <- arm(name = 'dose = 0.5')
  trt1$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 1),
                 generator = rbinom, size = 1, prob = .208)
  trt2 <- arm(name = 'dose = 1.5')
  trt2$add_endpoints(ep)

  ep <- endpoint(name = 'ep', type = 'non-tte', readout = c(ep = 1),
                 generator = rbinom, size = 1, prob = .241)
  trt3 <- arm(name = 'dose = 2.5')
  trt3$add_endpoints(ep)

  ## add three new arms to trial
  trial$add_arms(sample_ratio = c(2, 2, 2), trt1, trt2, trt3)
  
}

action_at_final <- function(trial){

  locked_data <- trial$get_locked_data('final')

  ## NOTE: go_nogo() (DoseFinding::maFitMod) is the heavy step in the original
  ## vignette. Stubbed here so the test can use a larger n; the lock-time
  ## logic we are testing is unaffected by this action.
  trial$save(value = 'skipped', name = 'decision')

}

interim <- milestone(name = 'interim',
                     when = eventNumber(endpoint = 'ep', n = 30),
                     action = action_at_interim)

final <- milestone(name = 'final',
                   when = eventNumber(endpoint = 'ep', n = 150),
                   action = action_at_final)

# NA

# NA

listener <- listener()
listener$add_milestones(interim, final)

controller <- controller(trial, listener)
