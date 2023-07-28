
test_that("no competing_events node", {
  out <- get_ce_names(tx_node_names="test", tx_node_types="time_to_event")
  expect_true(is.null(out))
})

test_that("one competing events node", {
  out <- get_ce_names(tx_node_names="test", tx_node_types="competing_events")
  expect_equal(out, c("test_event", "test_time"))
})

test_that("two competing events nodes", {
  out <- get_ce_names(tx_node_names=c("test", "test1", "test2"),
                      tx_node_types=c("time_to_event", "competing_events",
                                      "competing_events"))
  expect_equal(out, c("test1_event", "test2_event",
                      "test1_time", "test2_time"))
})
