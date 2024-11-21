library(dplyr)
library(R6)
library(rlang)
library(survival)


## max number of enrolled patients
n_patients <- 1000

source('./example/DefinePlaceboArm.R')
source('./example/DefineLowDoseArm.R')
source('./example/DefineHighDoseArm.R')
source('./example/DefineFutilityEvent.R')
source('./example/DefinePfsInterimEvent.R')
source('./example/DefinePfsFinalEvent.R')
source('./example/DefineOsFinalEvent.R')

source('./example/DefineTrial.R')

listener <- Listener$new()
listener$add_events(
    futility_event,
    pfs_interim_event,
    pfs_final_event,
    os_final_event
  )

controller <- Controller$new(trial, listener)
controller$run()


