
#' Summary of Milestone Time from Simulated Trials
#'
#' @param output a data frame.
#' It assumes that triggering time of milestones
#' are store in columns \code{milestone_time_<...>}. It can be a data frame
#' returned by \code{controller$get_output()}, or row-binded from multiple
#' data frames returned by \code{controller$get_output()} (e.g., users may
#' run simulation under the \code{targets} framework).
#'
#' @returns
#' A data frame of class \code{milestone_time_summary}. It comes with a
#' \code{plot} method for visualization.
#'
#' @importFrom utils stack
#'
#' @examples
#'
#' # a minimum, meaningful, and executable example,
#' # where a randomized trial with two arms is simulated and analyzed.
#'
#' control <- arm(name = 'control arm')
#' active <- arm(name = 'active arm')
#'
#' pfs_in_control <- endpoint(name = 'PFS', type = 'tte', generator = rexp, rate = log(2) / 5)
#' control$add_endpoints(pfs_in_control)
#'
#' pfs_in_active <- endpoint(name = 'PFS', type = 'tte', generator = rexp, rate = log(2) / 6)
#' active$add_endpoints(pfs_in_active)
#'
#' accrual_rate <- data.frame(end_time = c(10, Inf), piecewise_rate = c(30, 50))
#' trial <- trial(name = 'trial',
#'                n_patients = 1000,
#'                duration = 40,
#'                enroller = StaggeredRecruiter,
#'                accrual_rate = accrual_rate,
#'                dropout = rweibull, shape = 2, scale = 38,
#'                silent = TRUE)
#'
#' trial$add_arms(sample_ratio = c(1, 1), control, active)
#'
#' action_at_final <- function(trial){
#'   locked_data <- trial$get_locked_data('final analysis')
#'   fitLogrank(Surv(PFS, PFS_event) ~ arm, placebo = 'control arm',
#'              data = locked_data, alternative = 'less')
#'   invisible(NULL)
#' }
#'
#' final <- milestone(name = 'final analysis',
#'                    action = action_at_final,
#'                    when = eventNumber(endpoint = 'PFS', n = 300))
#'
#' listener <- listener(silent = TRUE)
#' listener$add_milestones(final)
#'
#' controller <- controller(trial, listener)
#' controller$run(n = 10, plot_event = FALSE, silent = TRUE)
#'
#' output <- controller$get_output()
#' time <- summarizeMilestoneTime(output)
#' time
#'
#' plot(time)
#'
#'
#' @export
#'
summarizeMilestoneTime <- function(output){

  stopifnot(is.data.frame(output))
  cols <- grep('^milestone_time_<.*>', names(output), value = TRUE)

  if(length(cols) == 0){
    stop('No columns of milestone time can be found. ',
         'This function looks for columns milestone_time_<...>. ')
  }

  milestone_name <- sub('^milestone_time_<(.+)>$', '\\1', cols)

  res <- lapply(seq_along(cols),
                function(i){
                  vals <- output[[cols[i]]]
                  data.frame(
                    milestone = milestone_name[i],
                    mean = mean(vals, na.rm = TRUE),
                    median = median(vals, na.rm = TRUE),
                    sd = sd(vals, na.rm = TRUE),
                    n = sum(!is.na(vals))
                  )
                }) %>%
    bind_rows()

  class(res) <- c('milestone_time_summary', class(res))
  time <- output[cols]
  colnames(time) <- milestone_name
  attr(res, 'time') <- time
  res
}

#' Plot Triggering Time of Milestones in Simulated Trials
#'
#' @param x an object returned by \code{summarizeMilestoneTime()}.
#' @param ... currently not supported.
#'
#' @export
#'
plot.milestone_time_summary <- function(x, ...){

  df <- attr(x, 'time') %>% stack()

  p <- ggplot(df, aes(x = .data$ind, y = .data$values)) +
    geom_boxplot(fill = 'lightgreen') +
    labs(title = 'Distribution of Milestone Time',
         y = 'Time',
         x = 'Milestone') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(p)

}

