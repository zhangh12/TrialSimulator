% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitLogistic.R
\name{fitLogistic}
\alias{fitLogistic}
\title{Fit logistic regression model}
\usage{
fitLogistic(endpoint, placebo, data, alternative, ...)
}
\arguments{
\item{endpoint}{Character. Name of the endpoint in \code{data}.}

\item{placebo}{Character. String indicating the placebo in \code{data$arm}.}

\item{data}{Data frame. Usually it is a locked data set.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"greater"} or \code{"less"}. No default value.
\code{"greater"} means superiority of treatment over placebo is established
by an odds ratio greater than 1 because a
logistic regression model is fitted with \code{endpoint ~ I(arm != placebo)}.}

\item{...}{Subset conditions compatible with \code{dplyr::filter}.
\code{glm} will be fitted on this subset only. This argument can be useful
to create a subset of data for analysis when a trial consists of more
than two arms. By default, it is not specified,
all data will be used to fit the model. More than one condition can be
specified in \code{...}, e.g.,
\code{fitLogistic('remission', 'pbo', data, arm \%in\% c('pbo', 'low dose'), cfb > 0.5)},
which is equivalent to:
\code{fitLogistic('remission', 'pbo', data, arm \%in\% c('pbo', 'low dose') & cfb > 0.5)}.
Note that if more than one treatment arm are present in the data after
applying filter in \code{...}, models are fitted for placebo verse
each of the treatment arms.}
}
\value{
a data frame with three columns:
\describe{
\item{\code{p}}{one-sided p-value for log odds ratio (treated vs placebo). }
\item{\code{info}}{sample size in the subset with \code{NA} being removed. }
\item{\code{z}}{the z statistics of log odds ratio (treated vs placebo). }
}
}
\description{
Fit logistic regression model on an binary endpoint.
}
