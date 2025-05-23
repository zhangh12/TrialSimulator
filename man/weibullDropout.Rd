% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weibullDropout.R
\name{weibullDropout}
\alias{weibullDropout}
\title{Calculate Parameters of Weibull Distribution as a Dropout Method}
\usage{
weibullDropout(time, dropout_rate)
}
\arguments{
\item{time}{a numeric vector of two time points at which dropout rates are
specified.}

\item{dropout_rate}{a numeric vector of dropout rates at \code{time}.}
}
\value{
a named vector for scale and shape parameters.
}
\description{
Fit scale and shape parameters of the Weibull distribution to match
dropout rates at two specified time points.
}
