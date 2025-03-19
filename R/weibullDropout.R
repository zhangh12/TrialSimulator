
#' @export
weibullDropout <- function(time, dropout_rate){

  if(length(time) != 2){
    stop('time should be of length 2. ')
  }

  if(length(dropout_rate) != 2){
    stop('dropout_rate should be of length 2. ')
  }

  if(!(all(time > 0))){
    stop('time should be positive')
  }

  if(!(all(dropout_rate > 0) && all(dropout_rate < 1))){
    stop('dropout_rate should be between 0 and 1')
  }

  shape <- diff(log(-log(1 - dropout_rate))) / diff(log(time))
  scale <- exp(log(time[1]) - log(-log(1 - dropout_rate[1]))/shape)

  stopifnot(max(abs(1 - exp(-(time / scale)^shape) - dropout_rate)) < 1e-4)
  c(shape = shape, scale = scale)

}
