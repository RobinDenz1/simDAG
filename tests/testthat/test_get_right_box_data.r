
test_that("one node", {

  expected <- data.frame(x=1,
                         y=c(4, 2),
                         label=c("Box 1", "A"),
                         type=c("none", "time_varying_node"),
                         ymax=c(14, 12),
                         ymin=c(-6, -8),
                         xmax=11,
                         xmin=-9)

  data <- data.frame(x=0,
                     y=c(4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "Last Box"),
                     type=c("none", "none", "time_varying_node", "none"),
                     ymax=c(14, 13, 12, 11),
                     ymin=c(-6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_right_box_data(data=data,
                            node_labels="A",
                            box_width=10,
                            box_height=10,
                            box_hdist=1,
                            box_1_text_right="Box 1",
                            box_2_text="Box 2",
                            box_last_text="Last Box")
  rownames(out) <- NULL

  expect_equal(out, expected)
})

test_that("two nodes", {

  expected <- data.frame(x=1,
                         y=c(5, 3, 2),
                         label=c("Box 1", "A", "B"),
                         type=c("none", "time_varying_node",
                                "time_varying_node"),
                         ymax=c(15, 13, 12),
                         ymin=c(-5, -7, -8),
                         xmax=11,
                         xmin=-9)

  data <- data.frame(x=0,
                     y=c(5, 4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "B", "Last Box"),
                     type=c("none", "none", "time_varying_node",
                            "time_varying_node", "none"),
                     ymax=c(15, 14, 13, 12, 11),
                     ymin=c(-5, -6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_right_box_data(data=data,
                            node_labels=c("A", "B"),
                            box_width=10,
                            box_height=10,
                            box_hdist=1,
                            box_1_text_right="Box 1",
                            box_2_text="Box 2",
                            box_last_text="Last Box")
  rownames(out) <- NULL

  expect_equal(out, expected)
})
