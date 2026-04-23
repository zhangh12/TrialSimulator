# Output summaries
#
# Covers:
#   - summarizeMilestoneTime: extracts and summarizes milestone_time_<...> cols,
#     errors when none are present, attaches a `time` attribute usable by
#     plot(), plot.milestone_time_summary returns invisibly
#   - summarizeDataFrame: returns invisible HTML character string and handles
#     tte + categorical + numeric columns; validates tte/event vector lengths

test_that("summarizeMilestoneTime summarizes matching columns", {

  set.seed(11)
  output <- data.frame(
    seed = 1:10,
    `milestone_time_<interim>` = runif(10, 10, 20),
    `milestone_time_<final>`   = runif(10, 30, 40),
    extra_col                  = rnorm(10),
    check.names = FALSE
  )

  res <- summarizeMilestoneTime(output)
  expect_s3_class(res, "milestone_time_summary")
  expect_equal(sort(res$milestone), sort(c("interim", "final")))
  expect_named(res, c("milestone", "mean", "median", "sd", "n"))
  expect_true(all(res$n == 10))
  expect_equal(res$mean[res$milestone == "interim"],
               mean(output$`milestone_time_<interim>`))

  # `time` attribute must carry the original columns renamed
  tm <- attr(res, "time")
  expect_true(is.data.frame(tm))
  expect_equal(sort(colnames(tm)), sort(c("interim", "final")))
})

test_that("summarizeMilestoneTime errors when no matching columns exist", {
  expect_error(summarizeMilestoneTime(data.frame(a = 1:3, b = 4:6)),
               "milestone_time_")
  expect_error(summarizeMilestoneTime("not a data frame"))
})

test_that("plot.milestone_time_summary returns invisibly", {

  output <- data.frame(
    `milestone_time_<m1>` = 1:5,
    `milestone_time_<m2>` = 6:10,
    check.names = FALSE
  )
  res <- summarizeMilestoneTime(output)
  # just checking it runs
  pdf(NULL); on.exit(dev.off())
  expect_invisible(plot(res))
})

test_that("summarizeDataFrame produces HTML for mixed-type data", {

  skip_if_not_installed("survival")

  set.seed(123)
  n <- 50
  data <- data.frame(
    age = rnorm(n, 65, 10),
    gender = sample(c("M", "F"), n, replace = TRUE),
    time_to_death = rexp(n, .01),
    death = rbinom(n, 1, .6),
    type = sample(LETTERS[1:3], n, replace = TRUE)
  )

  html <- summarizeDataFrame(data,
                             tte_vars = "time_to_death",
                             event_vars = "death")
  expect_type(html, "character")
  expect_length(html, 1)
  expect_match(html, "<table")
  expect_match(html, "age")
  expect_match(html, "time-to-event")
})

test_that("summarizeDataFrame rejects mismatched tte/event vars", {
  expect_error(
    summarizeDataFrame(data.frame(a = 1:3, b = 1:3),
                       tte_vars = c("a", "b"),
                       event_vars = "b"),
    "same length"
  )
})

test_that("summarizeDataFrame respects exclude_vars and categorical_vars", {

  set.seed(5)
  data <- data.frame(
    x = rnorm(20),
    flag = rbinom(20, 1, 0.5),
    group = sample(c("A", "B"), 20, replace = TRUE),
    secret = 1:20
  )

  html <- summarizeDataFrame(data,
                             exclude_vars = "secret",
                             categorical_vars = "flag")
  expect_type(html, "character")
  expect_false(grepl("secret", html))
  expect_match(html, "flag")
})
