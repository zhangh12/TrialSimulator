% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendarTime.R
\name{calendarTime}
\alias{calendarTime}
\title{Triggering condition by calendar time}
\usage{
calendarTime(time)
}
\arguments{
\item{time}{numeric. Calendar time to trigger a milestone of a trial.}
}
\value{
an object of class `Condition`
}
\description{
Define a condition to trigger trial milestone by calendar time. The milestone will
be trigger when a trial has been running for at least the specified
duration. It can be used combined with conditions specified by
\link[TrialSimulator]{enrollment} and \link[TrialSimulator]{eventNumber}.
}
