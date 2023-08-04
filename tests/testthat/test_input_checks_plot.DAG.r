
dag <- empty_dag() +
  node("A", type="rnorm") +
  node("B", type="binomial", parents=c("A")) +
  node("C", type="gaussian", parents=c("A", "B"))

test_that("not a DAG", {
  expect_error(plot.DAG("A"))
})

test_that("DAG too small", {
  dag <- empty_dag() + node("A", type="rnorm")

  expect_error(plot(dag))
})

test_that("wrong node_names", {
  expect_error(plot(dag, node_names=c("A", "B")))
})

test_that("wrong node_size", {
  expect_error(plot(dag, node_size="A"))
})

test_that("wrong arrow_node_dist", {
  expect_error(plot(dag, arrow_node_dist="A"))
})

test_that("wrong theme", {
  expect_error(plot(dag, gg_theme="A"))
})
