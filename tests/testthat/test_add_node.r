
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
  # error with standard node() call
  expect_error(empty_dag() + node("A", "rnorm") + node("A", "rbernoulli"))

  # no error when node is time-varying
  expect_no_error(empty_dag() + node("A", "rnorm") + node_td("A", rnorm))

  # error when node is time-varying, but also of type "time_to_event" or
  # "competing_events"
  expect_error(empty_dag() + node("A", "rnorm") +
                 node_td("A", "time_to_event", prob_fun=0.1))
  expect_error(empty_dag() + node("A", "rnorm") +
                 node_td("A", "competing_events", prob_fun=0.1))
})

test_that("error when DAG would become cyclic", {
  expect_error({
    dag <- empty_dag() +
      node("X", type="rnorm") +
      node("A", type="binomial", formula= ~ -2 + B*1) +
      node("B", type="binomial", formula= ~ -3 + A*1)
  }, paste0("Adding node 'B' as specified is impossible, ",
            "because it would make the DAG cyclic through the path:\n",
            "A -> B -> A"))
})

test_that("works with multiple names", {
  dag1 <- empty_dag() + node(c("A", "B", "C"), type="rbernoulli")
  dag2 <- node(c("A", "B", "C"), type="rbernoulli") + empty_dag()

  expect_equal(dag1, dag2)
  expect_equal(names_DAG(dag1), c("A", "B", "C"))
  expect_equal(names_DAG(dag2), c("A", "B", "C"))
})
