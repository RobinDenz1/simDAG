
dag <- empty_dag() +
  node("A", "rbernoulli", p=0.1) +
  node("B", "binomial", parents=c("A"), betas=c(1), intercept=-1) +
  node_td("C", "time_to_event", p=0.1)

test_that("all names", {
  expected <- c("A", "B", "C")
  out <- names_DAG(dag, include_tx_nodes=TRUE)
  expect_equal(out, expected)
})

test_that("no tx_node names", {
  expected <- c("A", "B")
  out <- names_DAG(dag, include_tx_nodes=FALSE)
  expect_equal(out, expected)
})

test_that("node in both", {
  dag <- dag + node_td("A", "gaussian")

  expected <- c("A", "B", "C")
  out <- names_DAG(dag, include_tx_nodes=TRUE)
  expect_equal(out, expected)
})
