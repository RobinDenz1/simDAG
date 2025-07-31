
test_that("general test cases, one network", {

  # directly supplying the graph
  set.seed(234)
  g <- igraph::sample_smallworld(1, 10, 2, 0.5)

  dag <- empty_dag() +
    network("net1", net=g) +
    node(c("A", "B", "C"), type="rbernoulli", p=0.2, output="numeric") +
    node("X", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2 +
           net(mean(A))*0.5) +
    node("Z", type="gaussian", formula= ~ -1 + A*0.5 + I(B^2)*0.1 +
           A:B*0.2 + net(as.numeric(any(C==1)))*0.2, error=1)

  data <- sim_from_dag(dag, n_sim=10)

  expect_equal(round(mean(data$X), 3), 0.2)
  expect_equal(round(mean(data$Y), 3), 0.1)
  expect_equal(round(mean(data$Z), 3), -1.237)

  # passing only a function
  gen_network <- function(n_sim) {
    g <- igraph::sample_smallworld(1, n_sim, 2, 0.5)
    return(g)
  }

  set.seed(234)

  dag <- empty_dag() +
    network("net1", net=gen_network) +
    node(c("A", "B", "C"), type="rbernoulli", p=0.2, output="numeric") +
    node("X", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2 +
           net(mean(A))*0.5) +
    node("Z", type="gaussian", formula= ~ -1 + A*0.5 + I(B^2)*0.1 +
           A:B*0.2 + net(as.numeric(any(C==1)))*0.2, error=1)

  data2 <- sim_from_dag(dag, n_sim=10)

  expect_equal(data2, data)
})

test_that("using data.table syntax in net()", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y3", type="gaussian", formula= ~ 0 + net(.N)*1, error=0)
  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(round(mean(data$Y3), 3), 3)
  expect_equal(round(data$Y3), data$Y3)
})

test_that("indexing singular individuals in net(), possibly with NAs", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y1", type="gaussian", formula= ~ 0 + net(..neighbor..[2])*1,
         error=0) +
    node("Y2", type="gaussian", formula= ~ 0 + net(variable_A[2], na=10)*1,
         error=0)
  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(round(mean(data$Y2, na.rm=TRUE), 3), 1.15)
})

test_that("static network with discrete-time simulation", {

  set.seed(234)
  g <- igraph::sample_smallworld(1, 100, 2, 0.5)

  dag <- empty_dag() +
    network("net1", net=g) +
    node("Infected", type="rbernoulli", p=0.1, output="numeric") +
    node_td("Infected", type="binomial",
            formula= ~ -1 + net(sum(Infected))*0.1)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=10, save_states="all")
  data <- sim2data(sim, to="start_stop")

  expect_equal(round(mean(data$Infected, na.rm=TRUE), 3), 0.457)
})

# TODO: currently fails!
test_that("mixing net() and mixed terms in formula syntax", {

  skip()

  set.seed(234)
  g <- igraph::sample_smallworld(1, 100, 2, 0.5)

  var_corr <- matrix(c(0.5, 0.05, 0.05, 0.1), 2)

  dag <- empty_dag() +
    network("net1", net=g) +
    node(c("A", "B"), type="rnorm") +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10]) +
    node("C", type="rcategorical", probs=c(0.1, 0.2, 0.7), output="factor") +
    node("Y", type="gaussian", formula= ~ -2 + net(sum(A==1))*0.2 +
           net(mean(A))*0.5 + I(B^3)*0.2 + A:B*0.1 + (1 + A|E) + C1*0.3 +
           C2*-4 + A*1.5, error=1, var_corr=var_corr)

  data <- sim_from_dag(dag, n_sim=100)

  #expect_equal(round(mean(data$X), 3), 0.2)
  #expect_equal(round(mean(data$Y), 3), 0.1)
  #expect_equal(round(mean(data$Z), 3), -1.237)
})

test_that("general test cases, multiple networks", {

  set.seed(234)
  g1 <- igraph::sample_smallworld(1, 15, 2, 0.5)
  g2 <- igraph::sample_smallworld(1, 15, 2, 0.5)
  g3 <- igraph::sample_smallworld(1, 15, 2, 0.5)

  dag <- empty_dag() +
    network("net1", net=g1) +
    network("net2", net=g2) +
    network("net3", net=g3) +
    node(c("A", "B", "C"), type="rbernoulli", p=0.2, output="numeric") +
    node("X", type="binomial", formula= ~ -2 + net(sum(A==1), net="net1")*0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A==1), net="net3")*0.2 +
           net(mean(A), "net2")*0.5) +
    node("Z", type="gaussian", formula= ~ -1 + A*0.5 + I(B^2)*0.1 +
           A:B*0.2 + net(as.numeric(any(C==1)), net="net1")*0.2, error=1)

  data <- sim_from_dag(dag, n_sim=15)

  expect_equal(round(mean(data$X), 3), 0.333)
  expect_equal(round(mean(data$Y), 3), 0.067)
  expect_equal(round(mean(data$Z), 3), -0.661)
})

test_that("error / warning with wrong network size", {

  set.seed(234)
  g1 <- igraph::sample_smallworld(1, 15, 2, 0.5)

  dag <- empty_dag() +
    network("net1", net=g1) +
    node("A", type="rbernoulli", p=0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)

  # network too small
  expect_error({data <- sim_from_dag(dag, n_sim=20)})

  # network too large
  expect_error({data <- sim_from_dag(dag, n_sim=10)})
})

test_that("error with no network in DAG", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)

  expect_error({data <- sim_from_dag(dag, n_sim=10000)})
})

test_that("error when multiple networks are supplied without naming them", {

  set.seed(234)
  g <- igraph::sample_smallworld(1, 10, 2, 0.5)

  dag <- empty_dag() +
    network("net1", net=g) +
    network("net2", net=g) +
    node("A", type="rbernoulli", p=0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)

  expect_error({data <- sim_from_dag(dag, n_sim=10)})
})
