
test_that("node of unrelated type", {
  node <- node("test", type="custom", parents=c("A", "B"))
  node2 <- add_missing_parents(node)

  expect_equal(node, node2)
})

test_that("competing_events node", {
  node <- node("test", type="competing_events", parents=c("A", "B"))
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time"))
})

test_that("time_to_event node, no optional cols", {
  node <- node("test", type="time_to_event", parents=c("A", "B"))
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time"))
})

test_that("time_to_event node, with event_count", {
  node <- node("test", type="time_to_event", parents=c("A", "B"),
               event_count=TRUE)
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time",
                                "test_event_count"))
})

test_that("time_to_event node, with time_since_last", {
  node <- node("test", type="time_to_event", parents=c("A", "B"),
               time_since_last=TRUE)
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time",
                                "test_time_since_last"))
})

test_that("time_to_event node, with event_count and time_since_last", {
  node <- node("test", type="time_to_event", parents=c("A", "B"),
               event_count=TRUE, time_since_last=TRUE)
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time",
                                "test_time_since_last", "test_event_count"))
})

test_that("no error if supplied by user", {
  node <- node("test", type="time_to_event", parents=c("A", "B", "test_event"))
  node2 <- add_missing_parents(node)

  expect_equal(node2$parents, c(".id", "A", "B", "test_event", "test_time"))
})
