#' Define Endpoints
#'
#' @description
#' Define one or multiple endpoints. This is a user-friendly wrapper for
#' the class constructor \code{Endpoints$new}. Users who are not familiar with
#' the concept of classes may consider using this wrapper directly.
#'
#' Note that it is users' responsibility to assure that the units of
#' readout of non-tte endpoints, dropout time, and trial duration are consistent.
#'
#' @param name character vector. Name(s) of endpoint(s)
#' @param type character vector. Type(s) of endpoint(s) in \code{name}. It
#' supports \code{"tte"} for time-to-event endpoints, and \code{"non-tte"} for
#' all other types of endpoints (e.g., continuous, binary, categorical,
#' or repeated measurement. \code{TrialSimulator} will do some verification if
#' an endpoint is of type \code{"tte"}. However, no special
#' manipulation is done for non-tte endpoints.
#' \code{"baseline"} can be used for a non-tte endpoint that is observed at
#' randomization (e.g., a baseline covariate, biomarker, or subgroup
#' indicator). Its readout is \code{0} by definition and must \strong{not} be
#' specified in \code{readout}; doing so triggers an error.
#' @param readout numeric vector named by non-tte endpoint(s).
#' \code{readout} should be specified for every non-tte endpoint. For
#' example, \code{c(endpoint1 = 6, endpoint2 = 3)}, which means that it takes
#' 6 and 3 unit time to get readouts of \code{endpoint1} and \code{endpoint2}
#' of a patient since being randomized. For readouts of a longitudinal endpoint
#' being collected at baseline (\code{baseline}) and 2 (\code{ep1}), 4 (\code{ep2})
#' unit time, its \code{readout} can be set as
#' \code{c(baseline = 0, ep1 = 2, ep2 = 4)}.  Error message will be prompted
#' if \code{readout} is not named or is not specified for all non-tte endpoint,
#' or it is specified for any tte endpoints. If all
#' endpoints are tte, \code{readout} should be its default value \code{NULL}.
#' Endpoints of type \code{"baseline"} must be omitted from \code{readout}, as
#' their readout is \code{0}.
#' @param generator a RNG function. Its first argument must be \code{n},
#' number of patients. It must return a data frame of \code{n} rows.
#' It supports all univariate random number generators, like those in
#' \code{stats}, e.g., \code{stats::rnorm}, \code{stats::rexp}, etc.
#' that with \code{n} as the first argument for number of
#' observations. \code{generator} could be any custom functions as long as
#' (1) its first argument is \code{n}; and (2) it returns a vector of
#' length \code{n} or a data frame of \code{n} rows. Custom random number
#' generator can return data of more than one endpoint. This is useful
#' when users need to simulate correlated endpoints (e.g., longitudinal
#' endpoints, or PFS/OS). The column names of
#' returned data frame should match to the argument \code{name} exactly,
#' but order does not matter. If an endpoint
#' is of type \code{"tte"}, the custom \code{generator} should also return
#' a column as event indicator. The column name of event indicator is
#' \code{<endpoint name>_event}. For example, if \code{"pfs"} is \code{"tte"},
#' then custom \code{generator} should return at least two columns
#' \code{"pfs"} and \code{"pfs_event"}. Usually \code{pfs_event} can be
#' all 1s if no censoring. For other generators, e.g.,
#' \code{TrialSimulator::PiecewiseConstantExponentialRNG} and
#' \code{TrialSimulator::CorrelatedPfsAndOs4}, the event indicators could
#' take values 0/1 due to the nature of their algorithms.
#' Censoring can also be specified later in \code{trial()}
#' through its argument \code{dropout}. See \code{?Trials}.
#' Note that if covariates, e.g., biomarker, subgroup, are needed in
#' generating and analyzing trial data, they can and should be defined as
#' endpoints as well.
#' @param ... (optional) arguments of \code{generator}.
#'
#' @examples
#' set.seed(12345)
#' ## Example 1. Generate a time-to-event endpoint.
#' ## Two columns are returned, one for time, one for event (1/0, 0 for
#  ## censoring)
#' ## A built-in RNG function is used to handle piecewise constant exponential
#' ## distribution
#' risk <- data.frame(
#'   end_time = c(1, 10, 26.0, 52.0),
#'   piecewise_risk = c(1, 1.01, 0.381, 0.150) * exp(-3.01)
#' )
#'
#' pfs <- endpoint(name = 'pfs', type='tte',
#'                 generator = PiecewiseConstantExponentialRNG,
#'                 risk = risk, endpoint_name = 'pfs')
#'
#' # run it in R console to display a summary report
#' # event indicator takes values 0/1
#' pfs
#'
#' ## Example 2. Generate continuous and binary endpoints using R's built-in
#' ## RNG functions, e.g. rnorm, rexp, rbinom, etc.
#' ep1 <- endpoint(
#'          name = 'cd4', type = 'non-tte', generator = rnorm, readout = c(cd4=1),
#'          mean = 1.2)
#' ep2 <- endpoint(
#'          name = 'resp_time', type = 'non-tte', generator = rexp, readout = c(resp_time=0),
#'          rate = 4.5)
#' ep3 <- endpoint(
#'          name = 'orr', type = 'non-tte', readout = c(orr=3), generator = rbinom,
#'          size = 1, prob = .4)
#'
#' ep1 # run it in R console. Mean and sd should be comparable to (1.2, 1.0)
#'
#' ep2 # run it in R console. Median should be comparable to log(2)/4.5 = 0.154
#'
#' ep3 # run it in R console. Mean and sd should be comparable to 0.4 and 0.49
#'
#'
#' ## Example3: delayed effect
#' ## Use piecewise constant exponential random number generator
#' ## Baseline hazards are piecewise constant
#' ## Hazard ratios are piecewise constant, resulting a delayed effect.
#' ## Note that this example is for explaining the concept of "endpoint".
#' ## Generating endpoint data manually is not the recommended way to use this package.
#'
#' run <- TRUE
#'
#' if (!requireNamespace("survminer", quietly = TRUE)) {
#'   run <- FALSE
#'   message("Please install 'survminer' to run this example.")
#' }
#'
#' if (!requireNamespace("survival", quietly = TRUE)) {
#'   run <- FALSE
#'   message("Please install 'survival' to run this example.")
#' }
#'
#' if(run){
#' risk1 <- risk
#' ep1 <- endpoint(
#'   name = 'pfs', type='tte',
#'   generator = PiecewiseConstantExponentialRNG,
#'   risk=risk1, endpoint_name = 'pfs')
#'
#' risk2 <- risk1
#' risk2$hazard_ratio <- c(1, 1, .6, .4)
#' ep2 <- endpoint(
#'   name = 'pfs', type='tte',
#'   generator = PiecewiseConstantExponentialRNG,
#'   risk=risk2, endpoint_name = 'pfs')
#'
#' n <- 1000
#' tte <- rbind(ep1$get_generator()(n), ep2$get_generator()(n))
#' arm <- rep(0:1, each = n)
#' dat <- data.frame(tte, arm)
#' sfit <- survival::survfit(
#'   survival::Surv(time = pfs, event = pfs_event) ~ arm, dat)
#'
#' survminer::ggsurvplot(sfit,
#'            data = dat,
#'            pval = TRUE,  # Show p-value
#'            conf.int = TRUE,  # Show confidence intervals
#'            risk.table = TRUE,  # Add risk table
#'            palette = c("blue", "red"))
#'
#'
#' ## print summary reports for endpoint objects in console
#' ep1
#' ep2
#'
#' }
#'
#' ## Example 4: generate correlated pfs and os
#' ## See vignette('simulatePfsAndOsIdm') and vignette('simulatePfsAndOsGumbel')
#'
#' @return an object of R6 class \code{Endpoints}, representing one or more endpoints.
#'
#' @export
endpoint = function(
    name,
    type,
    readout = NULL,
    generator,
    ...
){

  ## "baseline" is user-facing sugar handled here; Endpoints$new() only
  ## recognizes "tte" and "non-tte". A baseline endpoint is a non-tte endpoint
  ## observed at randomization, i.e., with readout = 0. Recycle a length-1 type
  ## so baseline endpoints can be identified per-name (mirrors Endpoints$new).
  ## Only handle baseline when type length is compatible with name; otherwise
  ## leave it to Endpoints$new() to raise its own input-validation error.
  type_ <- type
  if(length(type_) == 1 && length(name) > 1){
    type_ <- rep(type_, length(name))
  }

  if(length(type_) == length(name) && any(type_ == 'baseline')){
    baseline_vars <- name[type_ == 'baseline']

    clash <- intersect(baseline_vars, names(readout))
    if(length(clash) > 0){
      stop('Endpoint(s) of type "baseline" are observed at randomization ',
           '(readout = 0) and must not be given a readout: <',
           paste0(clash, collapse = ', '), '>. ')
    }

    type <- type_
    type[type == 'baseline'] <- 'non-tte'
    readout <- c(readout,
                 setNames(rep(0, length(baseline_vars)), baseline_vars))
  }

  Endpoints$new(
    name = name,
    type = type,
    readout = readout,
    generator = generator,
    ...
  )

}
