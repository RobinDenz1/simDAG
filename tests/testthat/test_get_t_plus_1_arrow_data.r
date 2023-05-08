
test_that("general test case", {

  expected <- data.frame(x=-15,
                         y=3,
                         label="Box 2",
                         type="none",
                         ymax=13,
                         ymin=-7,
                         xmax=10,
                         xmin=-10,
                         yend=3,
                         xend=-10)

  data <- data.frame(x=0,
                     y=c(4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "Last Box"),
                     type=c("none", "none", "time_varying_node", "none"),
                     ymax=c(14, 13, 12, 11),
                     ymin=c(-6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_t_plus_1_arrow_data(box_data=data,
                                 box_width=10,
                                 arrow_left_pad=5,
                                 box_2_text="Box 2")
  rownames(out) <- NULL

  expect_equal(out, expected)
})
