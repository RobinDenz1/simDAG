
test_that("general test case empty_dag()", {

  expected <- list(root_nodes=list(), child_nodes=list(),
                   tx_nodes=list())
  class(expected) <- "DAG"

  out <- empty_dag()

  expect_equal(out, expected)
})

test_that("S3 print method with empty dag", {

  dag <- empty_dag()

  expect_snapshot_output(print(dag))
})

test_that("S3 print method with filled dag", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5)

  expect_snapshot_output(print(dag))
})

test_that("S3 summary method with empty dag", {

  dag <- empty_dag()

  expect_snapshot_output(summary(dag))
})

test_that("S3 summary method with filled dag", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5)

  expect_snapshot_output(summary(dag))
})
