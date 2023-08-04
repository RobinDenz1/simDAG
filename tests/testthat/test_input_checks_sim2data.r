
set.seed(3452)

test_that("not a DAG object", {
  expect_error(sim2data("a", to="long"))
})

dag <- empty_dag() +
  node_td("death", type="time_to_event", prob_fun=0.0001,
          event_duration=Inf)

sim <- sim_discrete_time(dag, n_sim=100, max_t=200)

test_that("wrong use_saved_states", {
  expect_error(sim2data(sim, to="long", use_saved_states=1))
})

test_that("use_saved_states='all' when not done", {
  expect_error(sim2data(sim, to="long", use_saved_states=TRUE))
})

test_that("wrong to", {
  expect_error(sim2data(sim, to="long2", use_saved_states=FALSE))
})

test_that("warning save_past_events", {
  dag <- empty_dag() +
    node_td("death", type="time_to_event", prob_fun=0.0001,
            event_duration=Inf, save_past_events=FALSE)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=200)

  expect_warning(sim2data(sim, to="long", use_saved_states=FALSE))
})

test_that("warning save_states='at_t'", {
  dag <- empty_dag() +
    node_td("death", type="time_to_event", prob_fun=0.0001,
            event_duration=Inf)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=200, save_states="at_t",
                           save_states_at=c(5, 10, 15))

  expect_warning(sim2data(sim, to="long"))
})
