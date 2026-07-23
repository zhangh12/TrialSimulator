# Test fixtures: design setups for C++/R rollback consistency

Each file in this directory is a self-contained simulation setup used by
`test-cpp-rollback.R`. The tests verify that the C++ lock-time fast path
and the pure-R fallback produce *identical* `controller$get_output()`.

## Two file forms

- `setup_<name>.R` (vignette-derived): sourcing the file leaves a global
  `controller` object in the current environment, ready to call `$run()`.
  All `trial(...)` calls use a fixed `seed` so the runs are reproducible.

- `doc_<name>.R` (TrialSimulatorDocuments-derived): sourcing the file
  defines a `simulate_<name>(n, seed)` function that builds and runs
  internally and returns the output data frame.

## Why no stored expected RDS

The tests run the SAME setup twice on the SAME branch -- once with
`options(trialsimulator.use_cpp = FALSE)` and once with TRUE -- and compare
the two outputs with `expect_identical`. This avoids version-pinning a
binary fixture into the repo.

The toggle itself was validated once (offline) against a fresh install
of v1.17.1 (pre-C++) on every fixture in this directory, confirming that
the R-fallback path is byte-identical to the original v1.17.1 R logic.

`setup_stopFollowupAccrual.R` is not vignette-derived: it exists to cover the
adaptation methods `stop_followup()` and `update_accrual_rate()` (v1.25/v1.26)
under the C++/R identical-path check.
