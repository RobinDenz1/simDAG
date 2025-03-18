
test_that("general test case", {
  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("C", type="binomial", parents=c("A", "B")) +
    node("D", type="gaussian", parents=c("A", "B", "C"))
  g <- as.igraph(dag)
  expect_true(igraph::is_igraph(g))

  # excluding root nodes / td nodes
  g <- as.igraph(dag, include_root_nodes=FALSE, include_td_nodes=FALSE)
  expect_true(igraph::is_igraph(g))
})


