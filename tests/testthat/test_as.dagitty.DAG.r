
test_that("general test case", {
  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("C", type="binomial", parents=c("A", "B")) +
    node("D", type="gaussian", parents=c("A", "B", "C"))
  g1 <- as.dagitty(dag)
  expect_true(inherits(g1, "dagitty"))
  expect_equal(nchar(g1), 51)

  # excluding root nodes / td nodes
  g2 <- as.dagitty(dag, include_root_nodes=FALSE, include_td_nodes=FALSE)
  expect_true(inherits(g2, "dagitty"))
  expect_equal(nchar(g2), 19)
})
