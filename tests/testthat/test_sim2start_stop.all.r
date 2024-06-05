
test_that("outcome centric with event_duration > 1", {

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node_td("random", type="time_to_event", prob_fun=0.001, event_duration=10,
            immunity_duration=50) +
    node_td("vaccination", type="time_to_event", prob_fun=0.01,
            event_duration=20, immunity_duration=21) +
    node_td("sickness", type="time_to_event", prob_fun=0.01,
            parents="vaccination_event", event_duration=10)

  set.seed(343)

  sim <- sim_discrete_time(dag, n_sim=1000, max_t=200, save_states="all")

  # same output regardless of save_states value
  out1 <- sim2data(sim, to="start_stop", use_saved_states=FALSE,
                   target_event="sickness")
  out2 <- sim2data(sim, to="start_stop", use_saved_states=TRUE,
                   target_event="sickness")
  expect_equal(out1, out2)
})

test_that("outcome centric with event_duration == 1", {

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node_td("random", type="time_to_event", prob_fun=0.001, event_duration=10,
            immunity_duration=50) +
    node_td("vaccination", type="time_to_event", prob_fun=0.01,
            event_duration=20, immunity_duration=21) +
    node_td("sickness", type="time_to_event", prob_fun=0.01,
            parents="vaccination_event", event_duration=1)

  set.seed(343)

  sim <- sim_discrete_time(dag, n_sim=1000, max_t=200, save_states="all")

  # same output regardless of save_states value
  out1 <- sim2data(sim, to="start_stop", use_saved_states=FALSE,
                   target_event="sickness")
  out2 <- sim2data(sim, to="start_stop", use_saved_states=TRUE,
                   target_event="sickness")
  expect_equal(out1, out2)
})
