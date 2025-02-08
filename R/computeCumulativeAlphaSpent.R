
#' @importFrom stats as.formula pnorm
#' @importFrom mvtnorm pmvnorm
computeCumulativeAlphaSpent <- function(critical_values, information_fraction){

  corr <- outer(information_fraction, information_fraction,
                function(x, y) sqrt(pmin(x, y) / pmax(x, y)))

  alpha_spent <- c()
  for(k in seq_along(critical_values)){
    if(k == 1){
      lower <- critical_values[k]
      upper <- Inf
    }else{
      lower <- c(rep(-Inf, k - 1), critical_values[k])
      upper <- c(critical_values[1:(k-1)], Inf)
    }

    alpha_spent[k] <- pmvnorm(lower = lower, upper = upper, sigma = corr[1:k, 1:k, drop = FALSE])

  }

  cumsum(alpha_spent)

}

