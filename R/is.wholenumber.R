
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){

  if(!is.numeric(x)){
    return(FALSE)
  }

  abs(x - round(x)) < tol

}
