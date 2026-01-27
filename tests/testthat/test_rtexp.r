
test_that("always above l", {
  set.seed(231234)
  values <- rtexp(n=1000000, rate=0.001, l=500)
  expect_true(all(values > 500))
})

test_that("works if l is not specified", {
  values <- rtexp(n=10000, rate=1.2)
  expect_true(all(values > 0))
})

test_that("error with <= 0 rate", {
  expect_error(rtexp(n=3, rate=c(0.1, 0.3, 0), l=2),
               "All values in 'rate' should be > 0.")
  expect_error(rtexp(n=3, rate=c(0.1, 0.3, -0.3), l=2),
               "All values in 'rate' should be > 0.")
})
