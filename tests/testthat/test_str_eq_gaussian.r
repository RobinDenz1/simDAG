
test_that("unspecified node", {
  test_node <- node("A", type="gaussian", parents=c("C", "D"))
  expect_equal(str_eq_gaussian(test_node), "A ~ N()")
})

test_that("one parent", {
  test_node <- node("A", type="gaussian", parents="C", betas=1.2,
                    intercept=-12, error=3)
  expect_equal(str_eq_gaussian(test_node), "A ~ N(-12 + 1.2*C, 3)")

  # link="inverse"
  test_node$link <- "inverse"
  expect_equal(str_eq_gaussian(test_node), "A ~ N(1 / (-12 + 1.2*C, 3))")
})

test_that("multiple parents", {
  test_node <- node("A", type="gaussian", parents=c("C", "D", "E"),
                    betas=c(1.2, 0.3, 7), intercept=-12, error=4)
  expect_equal(str_eq_gaussian(test_node),
               "A ~ N(-12 + 1.2*C + 0.3*D + 7*E, 4)")
})
