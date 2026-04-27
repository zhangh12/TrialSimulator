


library(TrialSimulator)

simulate_example1 <- function(n = 1, seed = NULL){
  #' median PFS: 60 months in control
  pfs <- endpoint(name = 'pfs', type = 'tte', 
                  generator = rexp, rate = log(2) / 60)
  
  #' define placebo arm
  pbo <- arm(name = 'control')
  pbo$add_endpoints(pfs)
  
  #' hazard ratio: 0.74
  pfs <- endpoint(name = 'pfs', type = 'tte', 
                  generator = rexp, rate = log(2) / 60 * .74)
  
  #' define treatment arm
  trt <- arm(name = 'treatment')
  trt$add_endpoints(pfs)
  
  #' recruitment
  accrual_rate <- data.frame(end_time = c(1:6, Inf), 
                             piecewise_rate = seq(6, 42, by = 6))
  
  #' create a trial
  trial <- trial(
    name = 'Trial-1234', seed = seed, 
    n_patients = 1200, duration = 100, 
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rexp, rate = -log(1 - .025)/12
  )
  
  trial$add_arms(sample_ratio = c(1, 1), pbo, trt)
  
  interim_action <- function(trial){
    locked_data <- trial$get_locked_data('interim analysis')
    
    trial$bind(fitCoxph(Surv(pfs, pfs_event) ~ arm, 
                        placebo = 'control', 
                        data = locked_data, 
                        alternative = 'less', 
                        scale = 'log hazard ratio'), 
               name = 'stats')
  }
  
  interim <- milestone(name = 'interim analysis', 
                       when = eventNumber(endpoint = 'pfs', n = 232), 
                       action = interim_action)
  
  final_action <- function(trial){
    locked_data <- trial$get_locked_data('final analysis')
    
    trial$bind(fitCoxph(Surv(pfs, pfs_event) ~ arm, 
                        placebo = 'control', 
                        data = locked_data, 
                        alternative = 'less', 
                        scale = 'log hazard ratio'), 
               name = 'stats')
    
    gst <- GroupSequentialTest$new(
      alpha = .025, 
      alpha_spending = 'asOF', 
      planned_max_info = 351
    )
    
    stats <- trial$get_custom_data('stats')
    
    gst$test(
      observed_info = stats$info, 
      is_final = c(FALSE, TRUE), 
      p_values = stats$p
    )
    
    ret <- gst$get_trajectory()$decision
    if(ret[1] == 'reject'){
      ret[2] <- 'reject'
    }
    
    trial$save(ret[1], name = 'decision_<interim>')
    trial$save(ret[2], name = 'decision_<final>')
    
  }
  
  final <- milestone(name = 'final analysis', 
                     when = eventNumber(endpoint = 'pfs', n = 351), 
                     action = final_action)
  
  
  listener <- listener()
  listener$add_milestones(interim, final)
  
  controller <- controller(trial, listener)
  controller$run(n = n, plot_event = FALSE, silent = TRUE)
  
  controller$get_output()
  
}

