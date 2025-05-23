% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rconst.R
\name{rconst}
\alias{rconst}
\title{Generate constant variable}
\usage{
rconst(n, value)
}
\arguments{
\item{n}{integer. Number of observations.}

\item{value}{value of constant observations.}
}
\description{
A random number generator returning only a constant. This can be used to
set dropout time. Currently it is default value of dropout time, with
`value = Inf`.
}
