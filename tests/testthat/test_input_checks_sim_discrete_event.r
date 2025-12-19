
test_that("error with internal node name", {
  dag <- empty_dag() +
    node_td(".time", type="next_time", prob_fun=0.1)
  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf))
})

test_that("redraw at t correct input", {
  dag <- empty_dag() +
    node_td("something", type="next_time", prob_fun=0.1)
  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf,
                                  redraw_at_t=c(-10, 20)),
               paste0("'redraw_at_t' must be either NULL or a numeric ",
                      "vector containing only positive numbers."))
})

test_that("no time-varying variables in DAG", {
  dag <- empty_dag() +
    node("A", type="rnorm")
  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf))
})

test_that("time-dependent nodes other than next_time included", {
  dag <- empty_dag() +
    node("A", type="rnorm") +
    node_td("sssss", type="time_to_event", prob_fun=0.01, parents="A")
  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf))
})
