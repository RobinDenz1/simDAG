
test_that("constant probability", {
  out <- rbernoulli(n=5, p=0.1)
  expect_true(is.logical(out))
})

test_that("vector of probabilities", {
  out <- rbernoulli(n=5, p=c(0.1, 0.2, 0.3, 0.4, 0.5))
  expect_true(is.logical(out))
})

test_that("output numeric", {
  out <- rbernoulli(n=5, p=0.1, output="numeric")
  expect_true(is.numeric(out))
})

test_that("output character", {
  out <- rbernoulli(n=5, p=0.1, output="character")
  expect_true(is.character(out))
})

test_that("output factor", {
  out <- rbernoulli(n=5, p=0.1, output="factor")
  expect_true(is.factor(out))
})
