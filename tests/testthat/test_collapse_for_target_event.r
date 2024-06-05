
test_that("standard should be shortened", {
  # nothing special about that one event time, apart from it being
  # an event time (no covariate changed)
  d_test <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 10, 1, 4, 8, 11),
                       stop=c(2, 3, 9, 20, 3, 7, 18, 20),
                       covar=c(TRUE, TRUE, TRUE, FALSE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, FALSE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  expected <- data.table(.id=c(1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 4, 10, 1, 4, 8, 11),
                         stop=c(3, 9, 20, 3, 7, 18, 20),
                         covar=c(TRUE, TRUE, FALSE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(TRUE, FALSE, FALSE,
                                  FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, "target")
  expect_equal(out, expected)
})

test_that("standard should not be shortened", {
  # the event time coincides with a clear covariate change
  d_test <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 10, 1, 4, 8, 11),
                       stop=c(2, 3, 9, 20, 3, 7, 18, 20),
                       covar=c(TRUE, FALSE, TRUE, FALSE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, FALSE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, "target")
  expect_equal(d_test, out)
})

test_that("event at the start", {
  # this should not be shortened
  d_test <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 2, 4, 10, 1, 4, 8, 11),
                       stop=c(1, 3, 9, 20, 3, 7, 18, 20),
                       covar=c(TRUE, TRUE, FALSE, TRUE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(TRUE, FALSE, FALSE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, "target")
  expect_equal(d_test, out)
})

test_that("event at the end", {
  # event right at the end that should be shortened
  d_test <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2, 2),
                       start=c(1, 2, 4, 10, 1, 4, 8, 11, 21),
                       stop=c(1, 3, 9, 20, 3, 7, 18, 20, 21),
                       covar=c(TRUE, TRUE, FALSE, TRUE,
                               FALSE, TRUE, FALSE, TRUE, TRUE),
                       target=c(TRUE, FALSE, FALSE, FALSE,
                                FALSE, FALSE, FALSE, FALSE, TRUE))
  expected <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 2, 4, 10, 1, 4, 8, 11),
                         stop=c(1, 3, 9, 20, 3, 7, 18, 21),
                         covar=c(TRUE, TRUE, FALSE, TRUE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(TRUE, FALSE, FALSE, FALSE,
                                  FALSE, FALSE, FALSE, TRUE))

  out <- collapse_for_target_event(d_test, "target")
  expect_equal(out, expected)
})

test_that("multiple events per person that should be shortened", {
  # both events should be shortened
  d_test <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 10, 11, 1, 4, 8, 11),
                       stop=c(2, 3, 9, 10, 20, 3, 7, 18, 20),
                       covar=c(TRUE, TRUE, TRUE, TRUE, FALSE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, FALSE, TRUE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  expected <- data.table(.id=c(1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 4, 11, 1, 4, 8, 11),
                         stop=c(3, 10, 20, 3, 7, 18, 20),
                         covar=c(TRUE, TRUE, FALSE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(TRUE, TRUE, FALSE,
                                  FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, target_event="target")
  expect_equal(out, expected)
})

test_that("multiple events per person that should not be shortened", {
  d_test <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 10, 11, 1, 4, 8, 11),
                       stop=c(2, 3, 9, 10, 20, 3, 7, 18, 20),
                       covar=c(TRUE, FALSE, TRUE, FALSE, FALSE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, FALSE, TRUE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, target_event="target")
  expect_equal(out, d_test)
})

test_that("two events right after each other", {
  d_test <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 5, 1, 4, 8, 11),
                       stop=c(2, 3, 4, 20, 3, 7, 18, 20),
                       covar=c(TRUE, TRUE, TRUE, TRUE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, TRUE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  expected <- data.table(.id=c(1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 4, 5, 1, 4, 8, 11),
                         stop=c(3, 4, 20, 3, 7, 18, 20),
                         covar=c(TRUE, TRUE, TRUE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(TRUE, TRUE, FALSE,
                                  FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, "target")
  expect_equal(out, expected)
})

test_that("three events right after each other", {
  d_test <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 5, 6, 1, 4, 8, 11),
                       stop=c(2, 3, 4, 5, 20, 3, 7, 18, 20),
                       covar=c(TRUE, TRUE, TRUE, TRUE, TRUE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, TRUE, TRUE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  expected <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 4, 5, 6, 1, 4, 8, 11),
                         stop=c(3, 4, 5, 20, 3, 7, 18, 20),
                         covar=c(TRUE, TRUE, TRUE, TRUE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(TRUE, TRUE, TRUE, FALSE,
                                  FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, "target")
  expect_equal(out, expected)
})

test_that("one event starting right after it ends, with no event to shorten", {
  d_test <- data.table(.id=c(1, 1, 1, 1),
                       start=c(1, 7, 11, 12),
                       stop=c(6, 10, 11, 30),
                       A=c(TRUE, TRUE, TRUE, FALSE),
                       B=c(FALSE, FALSE, TRUE, TRUE),
                       C=c(FALSE, FALSE, TRUE, FALSE))
  out <- collapse_for_target_event(d_test, target_event="C")
  expect_equal(d_test, out)
})

test_that("one event starting right after it ends, with one event to shorten", {
  d_test <- data.table(.id=c(1, 1, 1, 1, 1, 1),
                       start=c(1, 3, 4, 7, 11, 12),
                       stop=c(2, 3, 6, 10, 11, 30),
                       A=c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
                       B=c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
                       C=c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  expected <- data.table(.id=c(1, 1, 1, 1, 1),
                         start=c(1, 4, 7, 11, 12),
                         stop=c(3, 6, 10, 11, 30),
                         A=c(TRUE, TRUE, TRUE, TRUE, FALSE),
                         B=c(FALSE, FALSE, FALSE, TRUE, TRUE),
                         C=c(TRUE, FALSE, FALSE, TRUE, FALSE))
  out <- collapse_for_target_event(d_test, target_event="C")
  expect_equal(out, expected)
})

test_that("only keeping time to first event", {
  d_test <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                       start=c(1, 3, 4, 10, 11, 1, 4, 8, 11),
                       stop=c(2, 3, 9, 10, 20, 3, 7, 18, 20),
                       covar=c(TRUE, FALSE, TRUE, FALSE, FALSE,
                               FALSE, TRUE, FALSE, TRUE),
                       target=c(FALSE, TRUE, FALSE, TRUE, FALSE,
                                FALSE, FALSE, FALSE, FALSE))
  expected <- data.table(.id=c(1, 1, 2, 2, 2, 2),
                         start=c(1, 3, 1, 4, 8, 11),
                         stop=c(2, 3, 3, 7, 18, 20),
                         covar=c(TRUE, FALSE,
                                 FALSE, TRUE, FALSE, TRUE),
                         target=c(FALSE, TRUE,
                                  FALSE, FALSE, FALSE, FALSE))
  out <- collapse_for_target_event(d_test, target_event="target",
                                   keep_only_first=TRUE)
  expect_equal(out, expected)
})
