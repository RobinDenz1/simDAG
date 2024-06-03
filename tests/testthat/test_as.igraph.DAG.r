
test_that("general test case", {
  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("C", type="binomial", parents=c("A", "B")) +
    node("D", type="gaussian", parents=c("A", "B", "C"))
  g <- as.igraph(dag)
  expect_true(igraph::is_igraph(g))
})
