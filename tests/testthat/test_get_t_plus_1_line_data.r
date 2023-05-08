
test_that("general test case", {

  expected <- data.frame(x=c(0, 0, -15),
                         xend=c(0, -15, -15),
                         y=c(-9, 10, 10),
                         yend=c(10, 10, 3))

  data <- data.frame(x=0,
                     y=c(4, 3, 2, 1),
                     label=c("Box 1", "Box 2", "A", "Last Box"),
                     type=c("none", "none", "time_varying_node", "none"),
                     ymax=c(14, 13, 12, 11),
                     ymin=c(-6, -7, -8, -9),
                     xmax=10,
                     xmin=-10)

  out <- get_t_plus_1_line_data(box_data=data,
                                box_vdist=1,
                                box_height=10,
                                box_width=10,
                                arrow_left_pad=5,
                                box_2_text="Box 2")
  rownames(out) <- NULL

  expect_equal(out, expected)
})
