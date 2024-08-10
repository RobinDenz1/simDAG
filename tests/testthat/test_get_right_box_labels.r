
test_that("one node", {

  expected <- "Probability dependent on:\n'1', '2', '4'"

  tx_nodes <- list(list(name="A",
                        type_str="time_to_event",
                        parents=c("1", "2", "4")))
  out <- get_right_box_labels(tx_names="A",
                              tx_nodes=tx_nodes)

  expect_equal(out, expected)
})

test_that("two nodes", {

  expected <- c("Probability dependent on:\n'1', '2', '4'",
                "Probability dependent on:\n'11', '12', '14'")

  tx_nodes <- list(list(name="A",
                        type_str="time_to_event",
                        parents=c("1", "2", "4")),
                   list(name="B",
                        type_str="time_to_event",
                        parents=c("11", "12", "14")))
  out <- get_right_box_labels(tx_names=c("A", "B"),
                              tx_nodes=tx_nodes)

  expect_equal(out, expected)
})

test_that("one competing events node", {

  expected <- "Change dependent on:\n'1', '2', '4'"

  tx_nodes <- list(list(name="A",
                        type_str="competing events",
                        parents=c("1", "2", "4")))
  out <- get_right_box_labels(tx_names="A",
                              tx_nodes=tx_nodes)

  expect_equal(out, expected)
})
