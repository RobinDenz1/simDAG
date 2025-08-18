
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
})

test_that("arguments passed through ... work", {

  set.seed(1324)

  gen_network <- function(n_sim, some_arg) {
    if (some_arg) {
      m <- 10
    } else {
      m <- 60
    }
    g <- igraph::sample_gnm(n=n_sim, m=m)
    return(g)
  }

  dag <- empty_dag() +
    network("net1", net=gen_network, some_arg=FALSE) +
    node("root", type="rnorm") +
    node("A", type="identity", formula= ~ net(.N), kind="data")
  data <- sim_from_dag(dag, n_sim=100, return_networks=TRUE)

  expect_equal(length(igraph::E(data$networks$net1$net)), 60)
})

test_that("one network, using a generating function", {

  gen_network <- function(n_sim) {
    g <- igraph::sample_smallworld(1, n_sim, 2, 0.5)
    return(g)
  }

  set.seed(234)

  dag <- empty_dag() +
    network("net1", net=gen_network, parents="") +
    node(c("A", "B", "C"), type="rbernoulli", p=0.2, output="numeric") +
    node("X", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2 +
           net(mean(A))*0.5) +
    node("Z", type="gaussian", formula= ~ -1 + A*0.5 + I(B^2)*0.1 +
           A:B*0.2 + net(as.numeric(any(C==1)))*0.2, error=1)

  data <- sim_from_dag(dag, n_sim=10)

  # NOTE: not equal to above test, because network is generated after
  #       the root nodes here
  expect_equal(round(mean(data$X), 3), 0)
  expect_equal(round(mean(data$Y), 3), 0.1)
  expect_equal(round(mean(data$Z), 3), -1.137)
})

test_that(".N equal to neighborhood size", {

  set.seed(1234)
  g <- igraph::sample_gnm(n=50, m=150)

  dag <- empty_dag() +
    node("A", type="rnorm") +
    network("net1", net=g) +
    node("n", type="identity", formula= ~ net(.N), kind="data")

  data <- sim_from_dag(dag, n_sim=50)
  data[, n_igraph := igraph::neighborhood_size(g, mindist=1)]

  expect_equal(data$n, data$n_igraph)
})

test_that("sorting with net() terms", {

  set.seed(234)
  g <- igraph::sample_smallworld(1, 10, 2, 0.5)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm") +
    node("F", type="binomial", formula= ~ 0.4 + net(mean(E))*0.3) +
    node("D", type="binomial", formula= ~ -2 + A*1 + B*0.1) +
    node("G", type="gaussian", formula= ~ -2 + net(mean(A))*0.3 +
           net(mean(D))*2, error=2) +
    node("E", type="binomial", formula= ~ 1.2 + D*0.2 + C*0.4) +
    network("net1", net=g)

  # does not work if not sorted first
  expect_error({data <- sim_from_dag(dag, n_sim=10)})

  # works with sort_dag=TRUE
  data <- sim_from_dag(dag, n_sim=10, sort_dag=TRUE)
  expect_true(ncol(data)==7)
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

test_that("using a different order of neighbors", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y1", type="gaussian", formula= ~ 0 + net(.N, order=1)*1, error=0) +
    node("Y2", type="gaussian", formula= ~ 0 + net(.N, order=2)*1, error=0) +
    node("Y3", type="gaussian", formula= ~ 0 + net(.N, order=3)*1, error=0)

  data <- sim_from_dag(dag, n_sim=20)

  expect_true(all(data$Y1 <= data$Y2))
  expect_true(all(data$Y2 <= data$Y3))

  # warning with weighted graphs
  igraph::E(g)$weight <- stats::runif(n=length(igraph::E(g)), min=0, max=5)

  expect_warning({
    dag <- empty_dag() +
      network("Net1", net=g) +
      node("variable_A", type="rnorm") +
      node("Y1", type="gaussian", formula= ~ 0 + net(.N, order=2)*1, error=0)
    data <- sim_from_dag(dag, n_sim=20)
  })
})

test_that("using different mindist in order > 1", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y1", type="gaussian",
         formula= ~ 0 + net(.N, order=2, mindist=0)*1, error=0) +
    node("Y2", type="gaussian",
         formula= ~ 0 + net(.N, order=2, mindist=1)*1, error=0) +
    node("Y3", type="gaussian",
         formula= ~ 0 + net(.N, order=2, mindist=2)*1, error=0) +
    node("Y4", type="gaussian",
         formula= ~ 0 + net(.N, order=3, mindist=2)*1, error=0)

  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(data$Y1, data$Y2)
  expect_equal(round(mean(data$Y2), 3), 9.6)
  expect_equal(round(mean(data$Y3), 3), 6.6)
  expect_equal(round(mean(data$Y4), 3), 13)
})

test_that("using a weighted network", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)
  igraph::E(g)$weight <- stats::runif(n=length(igraph::E(g)), min=0, max=5)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y1", type="gaussian", formula= ~ 0 +
           net(weighted.mean(x=variable_A, w=..weight..))*1, error=0) +
    node("Y2", type="gaussian", formula= ~ 0 +
           net(mean(variable_A))*1, error=0)
  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(round(mean(data$Y1), 3), -0.291)
  expect_equal(round(mean(data$Y2), 3), -0.235)
})

test_that("using a directed network", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30, directed=TRUE)
  igraph::E(g)$weight <- stats::runif(n=length(igraph::E(g)), min=0, max=5)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y_all", type="gaussian", formula= ~ 0 +
           net(.N, mode="all", na=0)*1, error=0) +
    node("Y_out", type="gaussian", formula= ~ 0 +
           net(.N, mode="out", na=0)*1, error=0) +
    node("Y_in", type="gaussian", formula= ~ 0 +
           net(.N, mode="in", na=0)*1, error=0) +
    node("Y1", type="gaussian", formula= ~ 0 +
           net(weighted.mean(x=variable_A, w=..weight..))*1, error=0) +
    node("Y2", type="gaussian", formula= ~ 0 +
           net(mean(variable_A))*1, error=0) +
    node("Y3", type="gaussian", formula= ~ 0 +
           net(weighted.mean(x=variable_A, w=..weight..), mode="out")*1,
         error=0)

  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(data$Y_all, data$Y_out + data$Y_in)
  expect_equal(round(mean(data$Y_out), 3), 1.5)
  expect_equal(round(mean(data$Y_in), 3), 1.5)

  expect_equal(round(mean(data$Y1), 3), -0.091)
  expect_equal(round(mean(data$Y2), 3), -0.063)

  expect_equal(round(mean(data$Y3), 3), -0.013)
})

test_that("using a directed network with weights", {

  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30, directed=TRUE)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("variable_A", type="rnorm") +
    node("Y_all", type="gaussian", formula= ~ 0 +
           net(.N, mode="all", na=0)*1, error=0) +
    node("Y_out", type="gaussian", formula= ~ 0 +
           net(.N, mode="out", na=0)*1, error=0) +
    node("Y_in", type="gaussian", formula= ~ 0 +
           net(.N, mode="in", na=0)*1, error=0)

  data <- sim_from_dag(dag, n_sim=20)

  expect_equal(data$Y_all, data$Y_out + data$Y_in)
  expect_equal(round(mean(data$Y_out), 3), 1.5)
  expect_equal(round(mean(data$Y_in), 3), 1.5)
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

test_that("network with a single net() call not dependent on other variables", {
  set.seed(2368)
  g <- igraph::sample_gnm(n=20, m=30)

  dag <- empty_dag() +
    network("Net1", net=g) +
    node("test", type="identity", formula= ~ net(.N), kind="data")
  expect_error({data <- sim_from_dag(dag, n_sim=20)})
})

test_that("network as a function of variables", {

  gen_network <- function(n_sim, data) {
    if (mean(data$age) > 10) {
      g <- igraph::sample_gnm(n=20, m=10)
    } else {
      g <- igraph::sample_gnm(n=20, m=50)
    }
    return(g)
  }

  set.seed(2368)
  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=1) +
    network("Net1", net=gen_network, parents="age") +
    node("Y3", type="gaussian", formula= ~ 0 + net(.N, na=0)*1, error=0)
  data1 <- sim_from_dag(dag, n_sim=20)

  set.seed(2368)
  dag <- empty_dag() +
    node("age", type="rnorm", mean=0, sd=1) +
    network("Net1", net=gen_network, parents="age") +
    node("Y3", type="gaussian", formula= ~ 0 + net(.N, na=0)*1, error=0)
  data2 <- sim_from_dag(dag, n_sim=20)

  # much higher number of neighbors when age is lower, because
  # generation of network was dependent on age
  expect_equal(round(mean(data1$Y3), 3), 1)
  expect_equal(round(mean(data2$Y3), 3), 5)
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

test_that("random dynamic network with discrete-time simulation", {

  prob_infection <- function(data, sim_time) {
    if (sim_time==1) {
      p <- rep(0.05, nrow(data))
    } else {
      p <- fifelse(data$n_infected_neighbors==0, 0,
                   fifelse(data$n_infected_neighbors > 3, 0.9, 0.4))
    }
    return(p)
  }

  gen_network <- function(n_sim) {
    igraph::sample_gnm(n=n_sim, m=30)
  }

  dag <- empty_dag() +
    node_td("n_infected_neighbors", type="gaussian",
            formula= ~ 0 + net(sum(infected_event), na=0)*1, error=0) +
    node_td("infected", type="time_to_event", event_duration=Inf,
            immunity_duration=Inf, parents=("n_infected_neighbors"),
            prob_fun=prob_infection) +
    network_td("net1", net=gen_network)

  set.seed(1335)
  sim <- sim_discrete_time(dag, n_sim=18, max_t=6, save_states="all",
                           save_networks=TRUE)
  data <- sim2data(sim, to="long")

  expect_true(length(sim$past_networks)==6)
  expect_true(igraph::is_igraph(sim$past_networks[[1]]$net1$net))
  expect_equal(round(mean(data$infected), 3), 0.454)
})

test_that("mix of static and dynamic networks in DTS", {

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
           A:B*0.2 + net(as.numeric(any(C==1)))*0.2, error=1) +
    node_td("Y2", type="binomial", formula= ~ -2 + net(sum(A==1))*0.2 +
              net(mean(A))*0.5)

  sim <- sim_discrete_time(dag, n_sim=10, max_t=5, save_states="all")
  data <- sim2data(sim, to="long")
  expect_equal(round(mean(data$Y2), 3), 0.08)
})

test_that("changing dynamic network with discrete-time simulation", {

  prob_infection <- function(data, sim_time) {
    if (sim_time==1) {
      p <- rep(0.05, nrow(data))
    } else {
      p <- fifelse(data$n_infected_neighbors==0, 0,
                   fifelse(data$n_infected_neighbors > 3, 0.9, 0.4))
    }
    return(p)
  }

  gen_network <- function(n_sim, sim_time, network, data,
                          past_states, past_networks) {

    if (sim_time==0) {
      return(igraph::sample_gnm(n=n_sim, m=23))
    }

    rm_edges <- data$.id[data$infected_event==TRUE &
                           data$infected_time_since_last > 0]

    if (length(rm_edges) > 0) {
      rm_edges <- do.call(c, igraph::incident_edges(network, rm_edges))
      g_new <- igraph::delete_edges(network, rm_edges)
    } else {
      g_new <- network
    }
    return(g_new)
  }

  dag <- empty_dag() +
    network_td("net1", net=gen_network, create_at_t0=TRUE) +
    node_td("n_infected_neighbors", type="gaussian",
            formula= ~ 0 + net(sum(infected_event), na=0)*1, error=0) +
    node_td("infected", type="time_to_event", event_duration=Inf,
            immunity_duration=Inf, parents=("n_infected_neighbors"),
            prob_fun=prob_infection, time_since_last=TRUE)

  set.seed(1335)
  sim <- sim_discrete_time(dag, n_sim=18, max_t=6, save_states="all",
                           save_networks=TRUE)
  data <- sim2data(sim, to="long")

  expect_true(length(sim$past_networks)==6)
  expect_true(igraph::is_igraph(sim$past_networks[[1]]$net1$net))
  expect_equal(round(mean(data$infected), 3), 0.241)
})

test_that("mixing net() and mixed terms in formula syntax", {

  set.seed(234)
  g <- igraph::sample_smallworld(1, 100, 2, 0.5)

  var_corr <- matrix(c(0.5, 0.05, 0.05, 0.1), 2)

  dag <- empty_dag() +
    network("net1", net=g) +
    node(c("A", "B"), type="rnorm") +
    node("E", type="rcategorical", probs=rep(0.1, 10), labels=LETTERS[1:10]) +
    node("C", type="rcategorical", probs=c(0.1, 0.2, 0.7), output="factor") +
    node("Y", type="gaussian", formula= ~ -2 + net(sum(A==1), na=1)*0.2 +
           net(mean(A), na=0)*0.5 + I(B^3)*0.2 + A:B*0.1 + (1 + A|E) + C1*0.3 +
           C2*-4 + A*1.5, error=1, var_corr=var_corr)

  data <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data$Y), 3), -5.3)
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

test_that("input errors", {

  set.seed(234)
  g1 <- igraph::sample_smallworld(1, 15, 2, 0.5)

  # wrong input to "net"
  expect_error({
    dag <- empty_dag() +
      network("net1", net="something")
  })

  # using a network directly in network_td()
  expect_error({
    dag <- empty_dag() +
      network_td("net1", net=g1)
  })

  # trying to specify multiple networks at once
  expect_error({
    dag <- empty_dag() +
      network(c("net1", "net2"), net=g1)
  })
})

test_that("error with wrong network size", {

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

test_that("error if generated network is not an igraph object", {

  gen_network <- function(n_sim) {
    return("something else")
  }

  dag <- empty_dag() +
    network("net", net=gen_network) +
    node("A", type="rbernoulli", p=0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)

  expect_error({data <- sim_from_dag(dag, n_sim=10000)})
})

test_that("error if network has < or > n_sim vertices", {

  gen_network <- function(n_sim) {
    g <- igraph::sample_gnm(n=40, m=20)
    return(g)
  }

  dag <- empty_dag() +
    network("net", net=gen_network) +
    node("A", type="rbernoulli", p=0.2) +
    node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)

  expect_error({data <- sim_from_dag(dag, n_sim=10000)})
})

test_that("error if network function does not have an 'n_sim' argument", {

  gen_network <- function(n) {
    g <- igraph::sample_gnm(n=n, m=20)
    return(g)
  }

  expect_error({
    dag <- empty_dag() +
      network("net", net=gen_network) +
      node("A", type="rbernoulli", p=0.2) +
      node("Y", type="binomial", formula= ~ -2 + net(sum(A))*0.2)
  })
})

test_that("error if network is generated only after being used", {

  gen_network <- function(n_sim) {
    igraph::sample_gnm(n=n_sim, m=n_sim*2)
  }

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node("C", type="binomial", formula= ~ -1 + A*0.2 + B*3 + net(mean(A))*0.3) +
    network("D", net=gen_network, parents=c("A", "B"))

  expect_error({data <- sim_from_dag(dag, n_sim=10)})

})

test_that("warning if internal variables are used", {

  gen_network <- function(n_sim, past_states) {
    igraph::sample_gnm(n=n_sim, m=n_sim*2)
  }

  expect_warning(network("Anet", net=gen_network, past_states=2))
  expect_warning(network_td("Anet", net=gen_network, past_states=2))
})
