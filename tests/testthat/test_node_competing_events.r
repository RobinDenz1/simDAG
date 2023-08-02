
test_that("general test case", {

  set.seed(3545)

  prob_death_illness <- function(data, sim_time) {

    # simply repeat the same probabilities for everyone
    n <- nrow(data)
    p_mat <- matrix(c(rep(0.9, n), rep(0.005, n), rep(0.005, n)),
                    byrow = FALSE, ncol=3)

    return(p_mat)
  }

  dag <- empty_dag() +
    node_td("death_illness", type="competing_events",
            prob_fun=prob_death_illness,
            event_duration=c(Inf, Inf))
  sim <- sim_discrete_time(dag, n_sim=10, max_t=50)

  expect_equal(colnames(sim$data), c(".id", "death_illness_event",
                                     "death_illness_time"))
  expect_true(nrow(sim$data)==10)

  ## making one of the event-types terminal and the other recurrent
  dag <- empty_dag() +
    node_td("death_illness", type="competing_events", prob_fun=prob_death_illness,
            event_duration=c(15, Inf))


  sim <- sim_discrete_time(dag, n_sim=10, max_t=50)

  expect_equal(colnames(sim$data), c(".id", "death_illness_event",
                                     "death_illness_time"))
  expect_true(nrow(sim$data)==10)
})
