#' Generate PFS, OS and objective response using the four-states model
#'
#' @param n integer. Number of observations.
#' @param transition_probability a 4x4 matrix defining transition probabilities
#' between stable (initial state, 1), response (2), progression (3) and
#' death (absorbing, 4).
#' @param duration integer. Duration of trial. Set it to a sufficient large
#' integer in practice to cover the duration of the trial (potentially be
#' extended).
#' @param death_name column name of OS in returned data frame. It must be
#' consistent with `name` in the function `endpoint()`.
#' @param progression_name column name of PFS in returned data frame. It must be
#' consistent with `name` in the function `endpoint()`.
#' @param response_name column name of objective response in returned data frame. It must be
#' consistent with `name` in the function `endpoint()`.
#'
#' @returns
#' A data frame of \code{n} rows and 6 columns (response, progression,
#' death, and their event indicators response_event, progression_event,
#' death_event with 1 means event and 0 means censored at duration).
#' @export
#'
#' @examples
#' m <- matrix(c(0.99, 0.0035, 0.0055, 0.0010,
#'                  0, 0.9900, 0.0052, 0.0048,
#'                  0,      0, 0.9960, 0.0040,
#'                  0,      0,      0,      1),
#'              nrow = 4, byrow = TRUE)
#'
#' pfs_and_os <- CorrelatedPfsAndOs4(1e4, m, 365 * 3)
#'
CorrelatedPfsAndOs4 <- function(n, transition_probability, duration,
                                death_name = 'death',
                                progression_name = 'progression',
                                response_name = 'response') {

  if(!is.matrix(transition_probability) ||
     !all(dim(transition_probability) == c(4, 4))) {
    stop("transition_probability must be a 4x4 matrix.")
  }

  if(any(transition_probability < 0)) {
    stop("All elements of transition_probability must be non-negative.")
  }

  row_sums <- rowSums(transition_probability)
  if(any(abs(row_sums - 1) > 1e-8)) {
    stop("Each row of transition_probability must sum to 1.")
  }

  if(any(lower.tri(transition_probability) & transition_probability != 0)) {
    stop("transition_probability must have zeros in lower triangle (M[i, j] = 0 for i > j).")
  }

  simulate_one_patient <- function(){

    first_visit <- c(1, rep(NA_integer_, 3))

    next_state_from_stable <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[1, ])

    day <- which(next_state_from_stable != 1 &
                   seq_along(next_state_from_stable) > 1)[1]
    if(is.na(day)){ ## stay in stable state

      return(first_visit[-1])
    }

    second_state <- next_state_from_stable[day]
    first_visit[second_state] <- day

    if(second_state == 4){ ## death
      if(is.na(first_visit[3])){
        first_visit[3] <- day
      }


      return(first_visit[-1])
    }

    third_states <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[second_state, ])

    day <- which(!(third_states %in% c(1, second_state)) &
                   seq_along(third_states) > day)[1]
    if(is.na(day)){ ## stay in second_state

      return(first_visit[-1])
    }

    third_state <- third_states[day]
    first_visit[third_state] <- day

    if(third_state == 4){ ## death
      if(is.na(first_visit[3])){
        first_visit[3] <- day
      }

      return(first_visit[-1])
    }

    fourth_states <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[third_state, ])

    day <- which(!(fourth_states %in% c(1, second_state, third_state)) &
                   seq_along(fourth_states) > day)[1]
    if(is.na(day)){ ## stay in third_state

      return(first_visit[-1])
    }

    fourth_state <- fourth_states[day]
    stopifnot(fourth_state == 4)
    first_visit[fourth_state] <- day


    return(first_visit[-1])
  }

  dat <- replicate(n, simulate_one_patient())

  dat <- as.data.frame(t(dat))
  colnames(dat) <- c('response', 'progression', 'death')

  stopifnot(with(dat, all(is.na(death) | death >= progression)))
  stopifnot(with(dat, all(is.na(progression) |
                            is.na(response) |
                            progression > response)))

  event <- dat
  event[is.na(dat)] <- 0
  event[!is.na(dat)] <- 1
  names(event) <- paste0(names(event), '_event')

  dat[is.na(dat)] <- duration

  dat <- cbind(dat, event)

  dat <- dat %>%
    rename(!!paste0(response_name, '_event') := .data$response_event) %>%
    rename(!!response_name := .data$response) %>%
    rename(!!paste0(progression_name, '_event') := .data$progression_event) %>%
    rename(!!progression_name := .data$progression) %>%
    rename(!!paste0(death_name, '_event') := .data$death_event) %>%
    rename(!!death_name := .data$death)

  dat

}



