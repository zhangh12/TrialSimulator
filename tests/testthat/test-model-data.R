model_recovery_data <- function() {
  data <- data.frame(
    arm = rep(c('pbo', 'trt1', 'trt2'), each = 80),
    x = rep(seq(1, 4, length.out = 80), 3),
    include = rep(c(TRUE, TRUE, FALSE, TRUE), 60)
  )
  data$ep <- sin(seq_len(nrow(data))) + 0.15 * log(data$x) +
    0.25 * (data$arm == 'trt1') + 0.4 * (data$arm == 'trt2')
  # Omitted outcomes and filtered rows must not influence the reference grid.
  data$ep[1] <- NA_real_
  data$x[c(1, 3)] <- 1000
  data$x[2] <- NA_real_
  data
}

test_that('fitLinear recovers transformed covariates from the fitted subset', {
  data <- model_recovery_data()
  result <- suppressMessages(fitLinear(
    ep ~ arm * log(x), placebo = 'pbo', data = data,
    alternative = 'greater', include
  ))

  expect_identical(result$arm, c('trt1', 'trt2'))
  for (trt in result$arm) {
    analysis_data <- data[data$include & data$arm %in% c('pbo', trt) &
                            complete.cases(data[c('ep', 'x')]), ]
    analysis_data$arm <- factor(analysis_data$arm, levels = c('pbo', trt))
    reference <- lm(ep ~ arm * log(x), data = analysis_data)
    predictions <- predict(reference, newdata = data.frame(
      arm = c('pbo', trt), x = mean(analysis_data$x)
    ))

    row <- result[result$arm == trt, ]
    expect_equal(row$estimate, unname(diff(predictions)), tolerance = 1e-8)
    expect_equal(row$info, nrow(analysis_data))
  }
})

test_that('fitLogistic recovers transformed covariates on every effect scale', {
  data <- model_recovery_data()
  scales <- c('log odds ratio', 'odds ratio', 'risk ratio', 'risk difference')

  for (scale in scales) {
    result <- suppressMessages(fitLogistic(
      I(ep > 0) ~ arm * log(x), placebo = 'pbo', data = data,
      alternative = 'greater', scale = scale, include
    ))

    expect_identical(result$arm, c('trt1', 'trt2'))
    for (trt in result$arm) {
      analysis_data <- data[data$include & data$arm %in% c('pbo', trt) &
                              complete.cases(data[c('ep', 'x')]), ]
      analysis_data$arm <- factor(analysis_data$arm, levels = c('pbo', trt))
      reference <- glm(I(ep > 0) ~ arm * log(x), data = analysis_data,
                       family = binomial())
      link <- unname(predict(reference, newdata = data.frame(
        arm = c('pbo', trt), x = mean(analysis_data$x)
      ), type = 'link'))
      probs <- plogis(link)
      estimate <- switch(scale,
        'log odds ratio' = diff(link),
        'odds ratio' = exp(diff(link)),
        'risk ratio' = probs[2] / probs[1],
        'risk difference' = diff(probs)
      )

      row <- result[result$arm == trt, ]
      expect_equal(row$estimate, estimate, tolerance = 1e-8)
      expect_equal(row$info, nrow(analysis_data))
    }
  }
})
