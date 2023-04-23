
test_that("2 nodes, 10 times", {
  example <- setup_past_events_list(names=c("A", "B"), max_t=10)
  expect_length(example, 2)
  expect_length(example[[1]], 10)
  expect_length(example[[2]], 10)
  expect_equal(names(example), c("A", "B"))
})

test_that("1 node, 10 times", {
  example <- setup_past_events_list(names="A", max_t=10)
  expect_length(example, 1)
  expect_length(example[[1]], 10)
  expect_error(example[[2]])
  expect_equal(names(example), "A")
})

test_that("1 node, 1 time", {
  example <- setup_past_events_list(names="A", max_t=1)
  expect_length(example, 1)
  expect_length(example[[1]], 1)
  expect_error(example[[2]])
  expect_equal(names(example), "A")
})

test_that("works with no nodes", {
  example <- setup_past_events_list(names=character(0), max_t=10)
  expect_length(example, 0)
  expect_error(example[[1]])
})
