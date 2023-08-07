
test_that("prob_fun args", {

  prob_car_crash <- function(data, sim_time, base_p) {
    return(base_p + sim_time * 0.0001)
  }

  dag <- empty_dag() +
    node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
            event_duration=1)

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3))
})
