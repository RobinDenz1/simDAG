
dag <- empty_dag() +
  node("A", type="rbernoulli", p=0.1) +
  node("B", type="rbernoulli", p=0.2) +
  node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
       intercept=-10, error=10) +
  node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
       intercept=-5)

test_that("all nodes", {

  expected <- matrix(c(0, 0, 1, 0, 0 ,0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0),
                     ncol=4, byrow=TRUE)
  colnames(expected) <- c("A", "B", "C", "D")
  rownames(expected) <- c("A", "B", "C", "D")

  out <- dag2matrix(dag=dag, include_root_nodes=TRUE)

  expect_equal(out, expected)
})

test_that("ignore root nodes", {

  expected <- matrix(c(0, 1, 0, 0), ncol=2, byrow=TRUE)
  colnames(expected) <- c("C", "D")
  rownames(expected) <- c("C", "D")

  out <- dag2matrix(dag=dag, include_root_nodes=FALSE)

  expect_equal(out, expected)
})
