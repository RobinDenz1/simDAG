
test_that("without argument names", {
  out <- get_param_str(list(mean=10, sd=3), use_names=FALSE)
  expect_equal(out, "10, 3")
})

test_that("with argument names", {
  out <- get_param_str(list(mean=10, sd=3), use_names=TRUE)
  expect_equal(out, "mean=10, sd=3")
})
