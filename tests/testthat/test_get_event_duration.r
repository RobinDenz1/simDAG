
node_with <- list(A=1, B=2, event_duration=25, immunity_duration=100)
node_without <- list(A=1, B=2)
node_with2 <- list(A=1, B=2, event_duration=25)

test_that("works with specified event_duration", {
  expect_equal(get_event_duration(node_with), 25)
})

test_that("takes default if not specified, event_duration", {
  expect_equal(get_event_duration(node_without), 1)
})

test_that("works with specified immunity_duration", {
  expect_equal(get_event_duration(node_with, type="immunity_duration"), 100)
})

test_that("takes default if not specified, immunity_duration", {
  expect_equal(get_event_duration(node_without, type="immunity_duration"), 1)
})

test_that("specified event_duration, but no specified immunity_duration", {
  expect_equal(get_event_duration(node_with2, type="immunity_duration"), 25)
})
