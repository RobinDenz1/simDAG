
test_that("error when not a DAG.node object", {
  expect_error(empty_dag() + 1)
  expect_error(add_node(empty_dag(), 1))
})

test_that("order doesn't matter", {
  first <- empty_dag() + node("A", "rbernoulli")
  second <- node("A", "rbernoulli") + empty_dag()

  expect_equal(first, second)
})

test_that("error when dag not a DAG", {
  expect_error("10" + node("A", "rnorm"))
})

test_that("not a DAG", {
  expect_error(add_node(dag="1", node=node("A", "rnorm")))
})

test_that("error when node name already in DAG", {
  expect_error(empty_dag() + node("A", "rnorm") + node("A", "rbernoulli"))
})
