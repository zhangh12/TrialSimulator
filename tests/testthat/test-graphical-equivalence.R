# GraphicalTesting: stage-wise testing equivalence
#
# The graph may be driven three ways that must agree:
#   - batch:       one gt$test(stats) call over all stages at once
#   - incremental: gt$test() called once per stage on a persisted object
#                  (the pattern used across milestones in a simulation)
#   - rebuild:     a fresh graph each stage, replaying all stages so far
#                  (the stateless pattern)
# These guard the documented "call gt$test(stats) once == call per stage"
# contract, including the alpha-propagation and within-stage re-test paths
# (the historically tricky case where a hypothesis is re-tested after a
# neighbour rejects and passes it alpha).

test_that("incremental == batch == rebuild across decision patterns", {
  skip_on_cran()

  hs  <- c('PFS_sub', 'PFS_all', 'OS_sub', 'OS_all')
  alp <- rep(0.00625, 4)                       # one-sided, FWER = 0.025
  tr  <- matrix(1/3, 4, 4); diag(tr) <- 0
  asf <- rep('asOF', 4)
  mx  <- c(300L, 400L, 350L, 450L)
  im  <- rbind(c(150, 225, 300), c(200, 300, 400),
               c(175, 260, 350), c(225, 340, 450))

  mk <- function(p) {
    data.frame(order = rep(1:3, 4), hypotheses = rep(hs, each = 3),
               p = p, info = as.vector(t(im)),
               is_final = rep(c(FALSE, FALSE, TRUE), 4),
               max_info = rep(mx, each = 3))
  }

  batch <- function(s) {
    g <- GraphicalTesting$new(alp, tr, asf, mx, hs, silent = TRUE)
    invisible(g$test(s))
    g$get_current_decision()
  }
  incremental <- function(s) {
    g <- GraphicalTesting$new(alp, tr, asf, mx, hs, silent = TRUE)
    d <- NULL
    for (m in sort(unique(s$order))) {
      invisible(g$test(filter(s, order == m)))
      d <- g$get_current_decision()
    }
    d
  }
  rebuild <- function(s) {
    d <- NULL
    for (m in sort(unique(s$order))) {
      g <- GraphicalTesting$new(alp, tr, asf, mx, hs, silent = TRUE)
      invisible(g$test(filter(s, order <= m)))
      d <- g$get_current_decision()
    }
    d
  }

  # one hypothesis (OS_all) is not tested at the interim-2 stage: rows only
  # at order 1 and 3 -- exercises omitted-stage handling.
  sm <- mk(c(.04, .008, .0011,  .5, .45, .4,  .03, .006, .0009,  .5, .45, .4))
  skip_middle <- sm[!(sm$hypotheses == 'OS_all' & sm$order == 2), ]

  scenarios <- list(
    all_accept  = mk(rep(c(.5, .4, .3), 4)),
    all_reject  = mk(rep(c(1e-4, 1e-5, 1e-6), 4)),
    # interim reject of H1 -> alpha propagates -> H2 crosses; H3/H4 accept
    propagate   = mk(c(1e-6, 1e-7, 1e-8,  .02, .004, .0009,  .5, .4, .3,  .5, .4, .3)),
    # cascade: two reject at interim, a third only at final
    cascade     = mk(c(1e-5, 1e-6, 1e-7,  1e-5, 1e-6, 1e-7,  .03, .01, .0015,  .5, .45, .4)),
    mixed       = mk(c(.04, .008, .0011,  .5, .45, .4,  .03, .006, .0009,  .5, .45, .4)),
    single_rej  = mk(c(.5, .4, .3,  .5, .4, .3,  1e-5, 1e-6, 1e-7,  .5, .4, .3)),
    final_only  = mk(c(.5, .4, .002,  .5, .4, .0015,  .5, .4, .3,  .5, .4, .3)),
    skip_middle = skip_middle
  )

  # incremental must match batch for every pattern (the core contract)
  for (nm in names(scenarios)) {
    s <- scenarios[[nm]]
    expect_identical(incremental(s), batch(s),
                     info = paste('incremental vs batch:', nm))
  }

  # the stateless rebuild path is costlier (it replays all stages each
  # milestone); check it on the patterns that exercise alpha propagation
  # and within-stage re-testing.
  for (nm in c('propagate', 'cascade', 'mixed', 'skip_middle')) {
    s <- scenarios[[nm]]
    expect_identical(rebuild(s), batch(s),
                     info = paste('rebuild vs batch:', nm))
  }
})
