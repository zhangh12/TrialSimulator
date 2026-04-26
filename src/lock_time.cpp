// Lock-time helpers used by EventCountCondition / EnrollmentCountCondition
// via Trials$get_data_lock_time_by_event_number() and ..._by_enrollment().
//
// All functions are pure deterministic transforms of the input vectors;
// no RNG calls. Behavior is bit-identical to the original R code path
// (which builds intermediate data.frames in get_event_tables() and then
// looks up the required calendar time).

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Earliest calendar time at which cumulative observed events >= target_n.
// cal_time[i] = enroll_time[i] + tte[i].
// Returns +Inf if the target cannot be reached.
//
// @param enroll_time numeric vector
// @param tte         numeric vector (time-to-event from enrollment, censored
//                    or not -- censoring is reflected via event_ind = 0)
// @param event_ind   integer vector of 0/1 event indicators
// @param target_n    integer, target cumulative event count
// @return earliest calendar time at which cumulative count reaches target_n
// [[Rcpp::export]]
double find_event_lock_time_cpp(NumericVector enroll_time,
                                NumericVector tte,
                                IntegerVector event_ind,
                                int target_n) {
  int n = enroll_time.size();
  if (n == 0) return R_PosInf;
  std::vector<std::pair<double,int>> v;
  v.reserve(n);
  for (int i = 0; i < n; i++) {
    int e = event_ind[i];
    if (e == NA_INTEGER) e = 0;
    v.emplace_back(enroll_time[i] + tte[i], e);
  }
  std::sort(v.begin(), v.end(),
            [](const std::pair<double,int>& a, const std::pair<double,int>& b){
              return a.first < b.first;
            });
  int cum = 0;
  for (int i = 0; i < n; i++) {
    cum += v[i].second;
    if (cum >= target_n) return v[i].first;
  }
  return R_PosInf;
}

// For non-TTE endpoints: among patients whose readout has been observed
// (i.e. !is.na(endpoint)), take the calendar time at which the target_n-th
// readout becomes available.
// [[Rcpp::export]]
double find_readout_lock_time_cpp(NumericVector enroll_time,
                                  NumericVector readout_time,
                                  LogicalVector observed,
                                  int target_n) {
  int n = enroll_time.size();
  std::vector<double> cal;
  cal.reserve(n);
  for (int i = 0; i < n; i++) {
    if (observed[i] == TRUE && !NumericVector::is_na(readout_time[i])) {
      cal.push_back(enroll_time[i] + readout_time[i]);
    }
  }
  if ((int)cal.size() < target_n) return R_PosInf;
  std::nth_element(cal.begin(), cal.begin() + target_n - 1, cal.end());
  return cal[target_n - 1];
}

// Calendar time at which the target_n-th patient is enrolled.
// [[Rcpp::export]]
double find_enrollment_lock_time_cpp(NumericVector enroll_time, int target_n) {
  int n = enroll_time.size();
  if (n < target_n) return R_PosInf;
  std::vector<double> v(enroll_time.begin(), enroll_time.end());
  std::nth_element(v.begin(), v.begin() + target_n - 1, v.end());
  return v[target_n - 1];
}
