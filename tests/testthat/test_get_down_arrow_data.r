
test_that("one node", {

  expected <- data.frame(x=0,
                         y=c(-6, -7, -8),
                         label=c("Box 1", "Box 2", "A"),
                         type=c("none", "none", "time_varying_node"),
                         ymax=c(14, 13, 12),
                         ymin=c(-6, -7, -8),
                         xmax=10,
                         xmin=-10,
                         yend=c(13, 12, 11),
                         xend=0)

  data <- data.frame(x=0,
                     y=c(4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "Last Box"),
                     type=c("none", "none", "time_varying_node", "none"),
                     ymax=c(14, 13, 12, 11),
                     ymin=c(-6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_down_arrow_data(box_data=data)
  rownames(out) <- NULL

  expect_equal(out, expected)
})

test_that("two nodes", {

  expected <- data.frame(x=0,
                         y=c(-5, -6, -7, -8),
                         label=c("Box 1", "Box 2", "A", "B"),
                         type=c("none", "none", "time_varying_node",
                                "time_varying_node"),
                         ymax=c(15, 14, 13, 12),
                         ymin=c(-5, -6, -7, -8),
                         xmax=10,
                         xmin=-10,
                         yend=c(14, 13, 12, 11),
                         xend=0)

  data <- data.frame(x=0,
                     y=c(5, 4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "B", "Last Box"),
                     type=c("none", "none", "time_varying_node",
                            "time_varying_node", "none"),
                     ymax=c(15, 14, 13, 12, 11),
                     ymin=c(-5, -6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_down_arrow_data(box_data=data)
  rownames(out) <- NULL

  expect_equal(out, expected)
})
