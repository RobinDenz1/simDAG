
test_that("general test case", {
  mat <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 0), ncol=3, byrow=TRUE)
  colnames(mat) <- c("age", "sex", "death")
  rownames(mat) <- c("age", "sex", "death")

  type <- list(age="rnorm", sex="rbernoulli", death="binomial")

  out <- matrix2dag(mat=mat, type=type)

  expected <- empty_dag() +
    node("age", type="rnorm") +
    node("sex", type="rbernoulli") +
    node("death", type="binomial", parents=c("age", "sex"))

  expect_equal(out, expected)
})
