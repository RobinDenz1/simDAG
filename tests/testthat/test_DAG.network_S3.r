
## print S3 method

test_that("print, undirected, unweighted network", {
  g <- igraph::sample_gnm(n=10, m=5)
  test_net <- network("A", net=g)
  expect_snapshot_output(print(test_net))
})

test_that("print, undirected, weighted network", {
  g <- igraph::sample_gnm(n=10, m=5)
  igraph::E(g)$weight <- stats::runif(length(igraph::E(g)))

  test_net <- network("A", net=g)
  expect_snapshot_output(print(test_net))
})

test_that("print, directed, unweighted network", {
  g <- igraph::sample_gnm(n=10, m=5, directed=TRUE)
  test_net <- network("A", net=g)
  expect_snapshot_output(print(test_net))
})

test_that("print, directed, weighted network", {
  g <- igraph::sample_gnm(n=10, m=5, directed=TRUE)
  igraph::E(g)$weight <- stats::runif(length(igraph::E(g)))

  test_net <- network("A", net=g)
  expect_snapshot_output(print(test_net))
})

test_that("print, using a network generating function", {

  gen_network <- function(n_sim) {
    igraph::sample_gnm(n=10, m=5, directed=TRUE)
  }

  test_net <- network("A", net=gen_network)
  expect_snapshot_output(print(test_net))
})

## summary S3 method

test_that("summary, undirected, unweighted network", {
  g <- igraph::sample_gnm(n=10, m=5)
  test_net <- network("A", net=g)
  expect_snapshot_output(summary(test_net))
})

test_that("summary, undirected, weighted network", {
  g <- igraph::sample_gnm(n=10, m=5)
  igraph::E(g)$weight <- stats::runif(length(igraph::E(g)))

  test_net <- network("A", net=g)
  expect_snapshot_output(summary(test_net))
})

test_that("summary, directed, unweighted network", {
  g <- igraph::sample_gnm(n=10, m=5, directed=TRUE)
  test_net <- network("A", net=g)
  expect_snapshot_output(summary(test_net))
})

test_that("summary, directed, weighted network", {
  g <- igraph::sample_gnm(n=10, m=5, directed=TRUE)
  igraph::E(g)$weight <- stats::runif(length(igraph::E(g)))

  test_net <- network("A", net=g)
  expect_snapshot_output(summary(test_net))
})

test_that("summary, using a network generating function", {

  gen_network <- function(n_sim) {
    igraph::sample_gnm(n=10, m=5, directed=TRUE)
  }

  test_net <- network("A", net=gen_network)
  expect_snapshot_output(summary(test_net))
})
