
node_with <- list(A=1, B=2, event_duration=25)
node_without <- list(A=1, B=2)

test_that("works with specified event_duration", {
  expect_equal(get_event_duration(node_with), 25)
})

test_that("takes default if not specified", {
  expect_equal(get_event_duration(node_without), 0)
})
