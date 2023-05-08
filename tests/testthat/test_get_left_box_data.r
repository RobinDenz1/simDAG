
test_that("one node", {

  expected <- data.frame(x=0,
                         y=c(4, 3, 2, 1),
                         label=c("Box 1", "Box 2", "A", "Last Box"),
                         type=c("none", "none", "time_varying_node", "none"),
                         ymax=c(14, 13, 12, 11),
                         ymin=c(-6, -7, -8, -9),
                         xmax=10,
                         xmin=-10)

  out <- get_left_box_data(node_labels="A",
                           box_width=10,
                           box_height=10,
                           box_vdist=1,
                           box_hdist=1,
                           box_1_text_left="Box 1",
                           box_2_text="Box 2",
                           box_last_text="Last Box")

  expect_equal(out, expected)
})

test_that("two nodes", {

  expected <- data.frame(x=0,
                         y=c(5, 4, 3, 2, 1),
                         label=c("Box 1", "Box 2", "A", "B", "Last Box"),
                         type=c("none", "none", "time_varying_node",
                                "time_varying_node", "none"),
                         ymax=c(15, 14, 13, 12, 11),
                         ymin=c(-5, -6, -7, -8, -9),
                         xmax=10,
                         xmin=-10)

  out <- get_left_box_data(node_labels=c("A", "B"),
                           box_width=10,
                           box_height=10,
                           box_vdist=1,
                           box_hdist=1,
                           box_1_text_left="Box 1",
                           box_2_text="Box 2",
                           box_last_text="Last Box")

  expect_equal(out, expected)
})
