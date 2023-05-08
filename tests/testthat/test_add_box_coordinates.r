
test_that("general test case", {
  example <- data.frame(x=rnorm(10),
                        y=rnorm(10))

  out <- add_box_coordinates(data=example, box_width=10, box_height=10)

  expect_equal(colnames(out), c("x", "y", "ymax", "ymin", "xmax", "xmin"))
})
