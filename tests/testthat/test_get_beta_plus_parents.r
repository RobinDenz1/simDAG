
test_that("one beta", {
  out <- get_beta_plus_parents(1, "A")
  expect_equal(out, "1*A")
})

test_that("multiple betas", {
  out <- get_beta_plus_parents(c(1, 2, 3, 4), c("A", "B", "C", "D"))
  expect_equal(out, "1*A + 2*B + 3*C + 4*D")
})
