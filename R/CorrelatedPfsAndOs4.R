#' Generate PFS and OS using the four-states model
#'
#' @param n integer. Number of observations.
#' @param transition_probability a 4x4 matrix defining transition probabilities
#' between stable (initial state, 1), response (2), progression (3) and
#' death (absorbing, 4).
#' @param duration integer. Duration of trial. Set it to a sufficient large
#' integer in practice to cover the duration of the trial (potentially be
#' extended).
#'
#' @returns
#' A data frame of \code{n} rows and 3 columns (time to response, progression,
#' and death).
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
CorrelatedPfsAndOs4 <- function(n, transition_probability, duration) {

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
    event <- rep(NA_integer_, 3)

    next_state_from_stable <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[1, ])

    day <- which(next_state_from_stable != 1 &
                   seq_along(next_state_from_stable) > 1)[1]
    if(is.na(day)){ ## stay in stable state
      browser()
      return(first_visit[-1])
    }

    second_state <- next_state_from_stable[day]
    first_visit[second_state] <- day

    if(second_state == 4){ ## death
      if(is.na(first_visit[3])){
        first_visit[3] <- day
      }

      browser()
      return(first_visit[-1])
    }

    third_states <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[second_state, ])

    day <- which(!(third_states %in% c(1, second_state)) &
                   seq_along(third_states) > day)[1]
    if(is.na(day)){ ## stay in second_state
      browser()
      return(first_visit[-1])
    }

    third_state <- third_states[day]
    first_visit[third_state] <- day

    if(third_state == 4){ ## death
      if(is.na(first_visit[3])){
        first_visit[3] <- day
      }
      browser()
      return(first_visit[-1])
    }

    fourth_states <- sample(1:4, size = duration, replace = TRUE, prob = transition_probability[third_state, ])

    day <- which(!(fourth_states %in% c(1, second_state, third_state)) &
                   seq_along(fourth_states) > day)[1]
    if(is.na(day)){ ## stay in third_state
      browser()
      return(first_visit[-1])
    }

    fourth_state <- fourth_states[day]
    stopifnot(fourth_state == 4)
    first_visit[fourth_state] <- day

    browser()
    return(first_visit[-1])
  }

  dat <- replicate(n, simulate_one_patient())

  dat <- as.data.frame(t(dat))
  colnames(dat) <- c('time_to_response', 'time_to_progression', 'time_to_death')

  stopifnot(with(dat, all(is.na(time_to_death) | time_to_death >= time_to_progression)))
  stopifnot(with(dat, all(is.na(time_to_progression) |
                            is.na(time_to_response) |
                            time_to_progression > time_to_response)))


  dat

}



