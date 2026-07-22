## C++ vs R-fallback: every supported design must produce identical
## get_output() under both options(trialsimulator.use_cpp = TRUE) (the default,
## C++ fast path) and options(trialsimulator.use_cpp = FALSE) (pure-R fallback).
##
## The fixtures/ directory contains self-contained simulation setups;
## see fixtures/README.md.
##
## All tests skip on CRAN: even at small n, several designs take
## non-trivial time, and these tests are not user-facing functionality
## checks but internal consistency checks for the C++ optimization.

run_setup_form <- function(setup_file, n) {
  e <- new.env(parent = globalenv())
  ## fixtures mirror vignette code (no silent = TRUE in trial()), so muffle
  ## their messages here, both at setup and during run (per-replicate reset()
  ## re-adds arms); sink() below only diverts stdout, not message()
  suppressMessages(source(setup_file, local = e))
  sink(tempfile()); on.exit(sink())
  suppressMessages(
    e$controller$run(n = n, silent = TRUE, plot_event = FALSE)
  )
  e$controller$get_output()
}

run_doc_form <- function(setup_file, fn_name, n, seed) {
  e <- new.env(parent = globalenv())
  suppressMessages(source(setup_file, local = e))
  sink(tempfile()); on.exit(sink())
  suppressMessages(do.call(e[[fn_name]], list(n = n, seed = seed)))
}

with_toggle <- function(value, expr) {
  old <- options(trialsimulator.use_cpp = value)
  on.exit(options(old))
  expr
}

check_identical_paths <- function(label, run_fn, args) {
  out_R <- with_toggle(FALSE, do.call(run_fn, args))
  out_C <- with_toggle(TRUE,  do.call(run_fn, args))
  expect_identical(out_R, out_C,
                   info = paste('R-fallback != C++ for', label))
}

## ---- vignette-style designs (controller built in setup file) -----------

test_that('adaptiveDesign: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('adaptiveDesign', run_setup_form,
    list(setup_file = test_path('fixtures', 'setup_adaptiveDesign.R'), n = 10))
})

test_that('crossoverWashout: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('crossoverWashout', run_setup_form,
    list(setup_file = test_path('fixtures', 'setup_crossoverWashout.R'), n = 10))
})

test_that('doseRanging: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('doseRanging', run_setup_form,
    list(setup_file = test_path('fixtures', 'setup_doseRanging.R'), n = 10))
})

test_that('fixedDesign: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('fixedDesign', run_setup_form,
    list(setup_file = test_path('fixtures', 'setup_fixedDesign.R'), n = 10))
})

test_that('responseAdaptive: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('responseAdaptive', run_setup_form,
    list(setup_file = test_path('fixtures', 'setup_responseAdaptive.R'), n = 10))
})

## ---- TrialSimulatorDocuments examples (function-form) -----------------

test_that('doc_example1: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('doc_example1', run_doc_form,
    list(setup_file = test_path('fixtures', 'doc_example1.R'),
         fn_name = 'simulate_example1', n = 10, seed = 101L))
})

test_that('doc_example2: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('doc_example2', run_doc_form,
    list(setup_file = test_path('fixtures', 'doc_example2.R'),
         fn_name = 'simulate_example2', n = 10, seed = 102L))
})

test_that('doc_example3: R-fallback and C++ produce identical output', {
  skip_on_cran()
  check_identical_paths('doc_example3', run_doc_form,
    list(setup_file = test_path('fixtures', 'doc_example3.R'),
         fn_name = 'simulate_example3', n = 10, seed = 103L))
})
