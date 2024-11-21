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

futility_event$trigger_event(trial)
pfs_interim_event$trigger_event(trial)
pfs_final_event$trigger_event(trial)






