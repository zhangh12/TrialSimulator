# Define a Trial

Define a trial. This is a user-friendly wrapper for the class
constructor `Trial$new()`. Users who are not familiar with the concept
of classes may consider using this wrapper directly.

Trial's name, planned size/duration, enrollment plan, dropout mechanism
and seeding are specified in this function. Note that many of these
parameters can be altered adaptively during a trial.

Note that it is users' responsibility to assure that the units of
dropout time, trial duration, and readout of non-tte endpoints are
consistent.

## Usage

``` r
trial(
  name,
  n_patients,
  duration,
  description = name,
  seed = NULL,
  enroller,
  dropout = NULL,
  silent = FALSE,
  ...
)
```

## Arguments

- name:

  character. Name of trial. Usually, hmm..., useless.

- n_patients:

  integer. Maximum (and initial) number of patients could be enrolled
  when planning the trial. It can be altered adaptively during a trial.

- duration:

  Numeric. Trial duration. It can be altered adaptively during a trial.

- description:

  character. Optional for description of the trial. By default it is set
  to be trial's `name`. Usually useless.

- seed:

  random seed. If `NULL`, seed is set for each simulated trial
  automatically and saved in output. It can be retrieved in the `seed`
  column in `$get_output()`. Setting it to be `NULL` is recommended. For
  debugging, set it to a specific integer.

- enroller:

  a function returning a vector enrollment time for patients. Its first
  argument `n` is the number of enrolled patients. Set it to
  `StaggeredRecruiter` can handle most of the use cases. See
  [`?TrialSimulator::StaggeredRecruiter`](https://zhangh12.github.io/TrialSimulator/reference/StaggeredRecruiter.md)
  for more information.

- dropout:

  a function returning a vector of dropout time for patients. It can be
  any random number generator with first argument `n`, the number of
  enrolled patients. Usually `rexp` if dropout rate is set at a single
  time point, or `rweibull` if dropout rates are set at two time points.
  See
  [`?TrialSimulator::weibullDropout`](https://zhangh12.github.io/TrialSimulator/reference/weibullDropout.md).

- silent:

  logical. `TRUE` to mute messages. However, warning message is still
  displayed. Usually set it to `TRUE` in formal simulation. Default:
  `FALSE`.

- ...:

  (optional) arguments of `enroller` and `dropout`.

## Examples

``` r
risk1 <- data.frame(
  end_time = c(1, 10, 26.0, 52.0),
  piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
)

pfs1 <- endpoint(name = 'pfs', type='tte',
          generator = PiecewiseConstantExponentialRNG,
          risk = risk1, endpoint_name = 'pfs')

orr1 <- endpoint(
  name = 'orr', type = 'non-tte',
  readout = c(orr=1), generator = rbinom,
  size = 1, prob = .4)

placebo <- arm(name = 'pbo')

placebo$add_endpoints(pfs1, orr1)

risk2 <- risk1
risk2$hazard_ratio <- .8

pfs2 <- endpoint(name = 'pfs', type='tte',
          generator = PiecewiseConstantExponentialRNG,
          risk = risk2, endpoint_name = 'pfs')

orr2 <- endpoint(
  name = 'orr', type = 'non-tte',
  generator = rbinom, readout = c(orr=3),
  size = 1, prob = .6)

active <- arm(name = 'ac')

active$add_endpoints(pfs2, orr2)

## Plan a trial, Trial-3415, of up to 100 patients.
## Enrollment time follows an exponential distribution, with median 5
trial <- trial(
  name = 'Trial-3415', n_patients = 100,
  seed = 31415926, duration = 100,
  enroller = rexp, rate = log(2) / 5)

trial
#>  ⚕⚕ Trial Name:  Trial-3415  
#>  ⚕⚕ Description:  Trial-3415  
#>  ⚕⚕ Number of Arms:  0  
#>  ⚕⚕ Registered Arms:    
#>  ⚕⚕ Sample Ratio:    
#>  ⚕⚕ Number of Patients:  100  
#>  ⚕⚕ Planned Duration:  100  
#>  ⚕⚕ Random Seed:  31415926  

trial$add_arms(sample_ratio = c(1, 2), placebo, active)
#> Arm(s) <pbo, ac> are added to the trial. 
#> Randomization is done for 100 potential patients. 
#> Data of 100 potential patients are generated for the trial with 2 arm(s) <pbo, ac>. 

## updated information after arms are registered
trial
#>  ⚕⚕ Trial Name:  Trial-3415  
#>  ⚕⚕ Description:  Trial-3415  
#>  ⚕⚕ Number of Arms:  2  
#>  ⚕⚕ Registered Arms:  pbo, ac  
#>  ⚕⚕ Sample Ratio:  1, 2  
#>  ⚕⚕ Number of Patients:  100  
#>  ⚕⚕ Planned Duration:  100  
#>  ⚕⚕ Random Seed:  31415926  
```
