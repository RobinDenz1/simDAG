
test_that("no time-varying variables", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3)

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=5))
})

test_that("correct save_states_at", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3) +
    node_td("C", type="time_to_event", prob_fun=0.01)

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3, save_states="at_t",
                                 save_states_at="never"))
})

test_that("correct t0_transform_fun", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3) +
    node_td("C", type="time_to_event", prob_fun=0.01)

  transform_fun <- function(data) {
    return(data)
  }

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3,
                                 t0_transform_fun=transform_fun,
                                 t0_transform_args=list(A=1)))
})

test_that("correct tx_transform_fun", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3) +
    node_td("C", type="time_to_event", prob_fun=0.01)

  transform_fun <- function(data) {
    return(data)
  }

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3,
                                 tx_transform_fun=transform_fun,
                                 tx_transform_args=list(A=1)))
})

test_that("correct tx_nodes_order", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3) +
    node_td("C", type="time_to_event", prob_fun=0.01) +
    node_td("D", type="time_to_event", prob_fun=0.1)

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3,
                                 tx_nodes_order=1))
})

test_that("wrong save_states", {
  dag <- empty_dag() +
    node("A", type="rnorm", mean=40, sd=10) +
    node("B", type="rbernoulli", p=0.3) +
    node_td("C", type="time_to_event", prob_fun=0.01) +
    node_td("D", type="time_to_event", prob_fun=0.1)

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3,
                                 save_states="never"))
})
