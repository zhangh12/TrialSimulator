library(dplyr)
library(R6)
library(rlang)
library(survival)

source('./example/DefinePlaceboArm.R')
source('./example/DefineLowDoseArm.R')
source('./example/DefineHighDoseArm.R')
source('./example/DefineTrial.R')
source('./example/DefineFutilityEvent.R')
source('./example/DefinePfsInterimEvent.R')
source('./example/DefinePfsFinalEvent.R')
source('./example/DefineOsFinalEvent.R')

trial_listener <- Listener$new()
trial_listener$add_events(
    futility_event,
    pfs_interim_event,
    pfs_final_event,
    os_final_event
  )

trial_listener$monitor(trial)

