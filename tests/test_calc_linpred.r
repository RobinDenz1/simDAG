
test_that("with n > 1 and p > 1", {

  data <- data.table(A=c(1, 2, 3),
                     B=c(TRUE, TRUE, FALSE),
                     C=c(0.1, 0.5, 3.1))
  betas <- c(0.1, -2, 2.1)
  intercept <- 0.3

  out <- calc_linpred(data=data, betas=betas, intercept=intercept)

  expect_equal(round(out, 2), c(-1.39, -0.45, 7.11))
})

test_that("with n > 1 and p = 1", {

  data <- data.table(A=c(1, 2, 3))
  betas <- c(0.1)
  intercept <- 0.3

  out <- calc_linpred(data=data, betas=betas, intercept=intercept)

  expect_equal(round(out, 2), c(0.4, 0.5, 0.6))
})

test_that("with n = 1 and p > 1", {

  data <- data.table(A=c(1),
                     B=c(TRUE),
                     C=c(0.1))
  betas <- c(0.1, -2, 2.1)
  intercept <- 0.3

  out <- calc_linpred(data=data, betas=betas, intercept=intercept)

  expect_equal(out, 0.3 + 0.1*1 + -2*1 + 2.1*0.1)
})

test_that("with n = 1 and p = 1", {

  data <- data.table(A=c(1))
  betas <- c(0.1)
  intercept <- 0.3

  out <- calc_linpred(data=data, betas=betas, intercept=intercept)

  expect_equal(out, 0.3 + 0.1*1)
})
