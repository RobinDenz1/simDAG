
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

test_that("ignore root nodes & allow td_nodes", {

  expected <- matrix(c(0, 1, 0, 0), ncol=2, byrow=TRUE)
  colnames(expected) <- c("C", "D")
  rownames(expected) <- c("C", "D")

  out <- dag2matrix(dag=dag, include_root_nodes=FALSE,
                    include_td_nodes=TRUE)

  expect_equal(out, expected)
})

test_that("including td_nodes, no doubles", {
  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5) +
    node_td("E", type="binomial", parents=c("A", "C"))

  expected <- matrix(c(0, 0, 1, 0, 1,
                       0 ,0, 1, 1, 0,
                       0, 0, 0, 1, 1,
                       0, 0, 0, 0, 0,
                       0, 0, 0, 0, 0),
                     ncol=5, byrow=TRUE)
  colnames(expected) <- c("A", "B", "C", "D", "E")
  rownames(expected) <- c("A", "B", "C", "D", "E")

  out <- dag2matrix(dag, include_td_nodes=TRUE)

  expect_equal(out, expected)
})

test_that("including td_nodes, with doubles", {
  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5) +
    node_td("B", type="binomial", parents=c("A", "C"))

  expected <- matrix(c(0, 1, 1, 0,
                       0 ,0, 1, 1,
                       0, 1, 0, 1,
                       0, 0, 0, 0),
                     ncol=4, byrow=TRUE)
  colnames(expected) <- c("A", "B", "C", "D")
  rownames(expected) <- c("A", "B", "C", "D")

  out <- dag2matrix(dag, include_td_nodes=TRUE)

  expect_equal(out, expected)
})

test_that("error: not a DAG object", {
  expect_error(dag2matrix(dag="1"))
})

test_that("error: wrong include_root_nodes", {
  expect_error(dag2matrix(dag=dag, include_root_nodes="A"))
})

test_that("only root nodes still numeric", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=34) +
    node("B", type="rcategorical")

  out <- dag2matrix(dag)

  expect_true(is.numeric(out) & !is.logical(out))
})
