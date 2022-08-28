
test_that("constant probability", {
  out <- rbernoulli(n=5, p=0.1)
  expect_true(is.logical(out))
})

test_that("vector of probabilities", {
  out <- rbernoulli(n=5, p=c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_true(is.logical(out))
})
