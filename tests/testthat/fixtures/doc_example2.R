

library(TrialSimulator)

simulate_example2 <- function(n = 1, seed = NULL){
  #' endpoint in control arm
  failure <- endpoint(name = 'failure', type = 'non-tte', 
                      generator = rbinom, size = 1, prob = .6, 
                      readout = c(failure = 0))
  
  pbo <- arm(name = 'control')
  pbo$add_endpoints(failure)
  
  failure <- endpoint(name = 'failure', type = 'non-tte', 
                      generator = rbinom, size = 1, prob = .4, 
                      readout = c(failure = 0))
  
  trt <- arm(name = 'treatment')
  trt$add_endpoints(failure)
  
  #' set arbitrary recruitment rate
  accrual_rate <- data.frame(end_time = c(1, Inf), 
                             piecewise_rate = c(10, 20))
  
  trial <- trial(
    name = 'Trial-1234', seed = seed, 
    n_patients = 266, duration = 1000, 
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate, 
    dropout = NULL)
  
  trial$add_arms(sample_ratio = c(1, 1), trt, pbo)
  
  action1 <- function(trial){
    locked_data <- trial$get_locked_data('interim 1')
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
  }
  
  action2 <- function(trial){
    locked_data <- trial$get_locked_data('interim 2')
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
    
  }
  
  action3 <- function(trial){
    locked_data <- trial$get_locked_data('final')
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
    stats <- trial$get_custom_data('stats')
    trial$save(stats$z[1] > 0, name = 'futility_<interim1>')
    trial$save(stats$z[2] > .5, name = 'futility_<interim2>')
    
    gst <- GroupSequentialTest$new(
      alpha = .025, 
      alpha_spending = 'asOF', 
      planned_max_info = 266
    )
    
    gst$test(
      observed_info = stats$info, 
      is_final = c(FALSE, FALSE, TRUE), 
      p_values = stats$p
    )
    
    ret <- gst$get_trajectory()$decision
    if(ret[1] == 'reject'){
      ret[-1] <- 'reject'
    }
    
    if(ret[2] == 'reject'){
      ret[3] <- 'reject'
    }
    
    trial$save(ret[1], name = 'decision_<interim1>')
    trial$save(ret[2], name = 'decision_<interim2>')
    trial$save(ret[3], name = 'decision_<final>')
    
  }
  
  interim1 <- milestone(name = 'interim 1', 
                        when = enrollment(n = 133), 
                        action = action1)
  
  interim2 <- milestone(name = 'interim 2', 
                        when = enrollment(n = 200), 
                        action = action2)
  
  final <- milestone(name = 'final', 
                     when = enrollment(n = 266), 
                     action = action3)
  
  listener <- listener()
  listener$add_milestones(interim1, interim2, final)
  
  controller <- controller(trial, listener)
  controller$run(n = n, plot_event = FALSE, silent = TRUE)
  
  controller$get_output()
  
}






