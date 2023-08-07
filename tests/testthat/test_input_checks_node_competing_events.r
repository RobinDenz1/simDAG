
test_that("prob_fun args", {
  prob_death_illness <- function(data, base_p) {

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

  expect_error(sim_discrete_time(dag, n_sim=10, max_t=3))
})
