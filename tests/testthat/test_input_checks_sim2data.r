
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

test_that("wrong overlap", {
  expect_error(sim2data(sim, to="long", overlap="left"),
               "'overlap' must be either TRUE or FALSE.")
})

test_that("wrong keep_only_first", {
  expect_error(sim2data(sim, to="long", keep_only_first=NULL),
               "'keep_only_first' must be either TRUE or FALSE.")
})

test_that("wrong target_event", {
  expect_error(sim2data(sim, to="long", target_event="sickness"),
               paste0("'target_event' must be a single character ",
                      "string, specifying a time_to_event node used ",
                      "in the creation of 'sim'."))
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
