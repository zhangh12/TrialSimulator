
#' Summary of Milestone Time from Simulated Trials
#'
#' @param output a data frame. It assumes that triggering time of milestones
#' are store in columns \code{milestone_time_<...>}. It can be data frames
#' returned by \code{controller$get_output()}.
#'
#' @returns
#' A data frame of class \code{milestone_time_summary}.
#'
#' @importFrom utils stack
#' @examples
#' \dontrun{
#' ## simulate trials
#' # ...
#' output <- controller$get_output()
#' time <- summarizeMilestoneTime(output)
#' time
#'
#' plot(time)
#' }
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
    do.call(rbind, .)

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

