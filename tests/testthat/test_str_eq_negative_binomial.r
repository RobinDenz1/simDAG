
test_that("unspecified node", {
  test_node <- node("A", type="negative_binomial", parents=c("C", "D"))
  expect_equal(str_eq_negative_binomial(test_node), "A ~ NegBinomial()")
})

test_that("one parent", {
  test_node <- node("A", type="negative_binomial", parents="C", betas=1.2,
                    intercept=-12, theta=0.2)
  expect_equal(str_eq_negative_binomial(test_node),
               "A ~ NegBinomial(-12 + 1.2*C + log(0.2))")
})

test_that("multiple parents", {
  test_node <- node("A", type="negative_binomial", parents=c("C", "D", "E"),
                    betas=c(1.2, 0.3, 7), intercept=-12, theta=0.3)
  expect_equal(str_eq_negative_binomial(test_node),
               "A ~ NegBinomial(-12 + 1.2*C + 0.3*D + 7*E + log(0.3))")
})
