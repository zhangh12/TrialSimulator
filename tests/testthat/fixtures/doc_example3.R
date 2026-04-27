

library(TrialSimulator)

simulate_example3 <- function(n = 1, seed = NULL){
  pfs <- endpoint(name = 'pfs', type = 'tte', 
                  generator = rexp, rate = -log(1-.2)/12)
  pbo <- arm(name = 'pbo')
  pbo$add_endpoints(pfs)
  
  pfs <- endpoint(name = 'pfs', type = 'tte', 
                  generator = rexp, rate = -log(1-.2)/12 * .85)
  trt <- arm(name = 'trt')
  trt$add_endpoints(pfs)
  
  trial <- trial(
    name = 'trial', seed = seed, 
    n_patients = 3000, duration = 100, 
    enroller = StaggeredRecruiter, 
    accrual_rate = data.frame(end_time = c(12, Inf), piecewise_rate = c(250, 250)), 
    dropout = rexp, rate = -log(1-.1)/12)
  
  trial$add_arms(sample_ratio = c(1, 1), trt, pbo)
  
  action1 <- function(trial){
    locked_data <- trial$get_locked_data('interim 1')
    trial$bind(fitLogrank(Surv(pfs, pfs_event) ~ arm, 
                          placebo = 'pbo', 
                          data = locked_data, 
                          alternative = 'less'), 
               name = 'stats')
  }
  
  action2 <- function(trial){
    locked_data <- trial$get_locked_data('interim 2')
    trial$bind(fitLogrank(Surv(pfs, pfs_event) ~ arm, 
                          placebo = 'pbo', 
                          data = locked_data, 
                          alternative = 'less'), 
               name = 'stats')
  }
  
  action3 <- function(trial){
    locked_data <- trial$get_locked_data('interim 3')
    trial$bind(fitLogrank(Surv(pfs, pfs_event) ~ arm, 
                          placebo = 'pbo', 
                          data = locked_data, 
                          alternative = 'less'), 
               name = 'stats')
  }
  
  action4 <- function(trial){
    locked_data <- trial$get_locked_data('final')
    trial$bind(fitLogrank(Surv(pfs, pfs_event) ~ arm, 
                          placebo = 'pbo', 
                          data = locked_data, 
                          alternative = 'less'), 
               name = 'stats')
    
    gst <- GroupSequentialTest$new(alpha = .025, alpha_spending = 'asOF', 
                                   planned_max_info = 1000)
    
    stats <- trial$get_custom_data('stats')
    
    gst$test(observed_info = stats$info, is_final = c(F,F,F,T), p_values = stats$p)
    
    ret <- gst$get_trajectory()$decision
    
    if(ret[1] == 'reject'){
      ret[-1] <- 'reject'
    }
    
    if(ret[2] == 'reject'){
      ret[3:4] <- 'reject'
    }
    
    if(ret[3] == 'reject'){
      ret[4] <- 'reject'
    }
    
    trial$save(ret[1], 'decision1')
    trial$save(ret[2], 'decision2')
    trial$save(ret[3], 'decision3')
    trial$save(ret[4], 'decision4')
  }
  
  interim1 <- milestone(name = 'interim 1', 
                        when = eventNumber(endpoint = 'pfs', n = 1000*.6), 
                        action = action1)
  
  interim2 <- milestone(name = 'interim 2', 
                        when = eventNumber(endpoint = 'pfs', n = 1000*.7), 
                        action = action2)
  
  interim3 <- milestone(name = 'interim 3', 
                        when = eventNumber(endpoint = 'pfs', n = 1000*.8), 
                        action = action3)
  
  final <- milestone(name = 'final', 
                     when = eventNumber(endpoint = 'pfs', n = 1000), 
                     action = action4)
  
  
  listener <- listener()
  listener$add_milestones(interim1, interim2, interim3, final)
  
  controller <- controller(trial, listener)
  controller$run(n = n, plot_event = FALSE, silent = TRUE)
  
  controller$get_output()
}

