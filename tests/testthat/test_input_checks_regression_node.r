
test_that("correct betas", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas="A", intercept=1, error=2))
})

test_that("correct intercept", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas=c(1, 2), intercept="1", error=2))
})

test_that("missing intercept", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas=c(1, 2), error=2))
})

test_that("missing betas", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    intercept=1, error=2))
})

test_that("betas wrong length", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas=1, error=2, intercept=1))
})

test_that("gaussian missing error", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas=c(1, 2), intercept=1))
})

test_that("wrong formula", {
  expect_error(node("A", type="gaussian", parents=c("B", "C"),
                    betas=c(1, 2), intercept=1, error=2,
                    formula=1))
})
