
prob_E <- function(data) {
  n <- nrow(data)
  p_mat <- matrix(c(rep(0.9, n), rep(0.005, n), rep(0.005, n)),
                  byrow=FALSE, ncol=3)
  return(p_mat)
}

dag <- empty_dag() +
  node("F", type="rbernoulli") +
  node_td(c("A", "B", "C"), type="time_to_event", prob_fun=0.01,
          event_duration=1) +
  node_td("E", type="competing_events", prob_fun=prob_E,
          event_duration=c(Inf, Inf)) +
  node_td("D", type="gaussian", formula= ~ -2 + A_event*3 + B_event*7 + F*-3,
          error=2)

set.seed(1234)
sim <- sim_discrete_time(dag, n_sim=100, max_t=10, save_states="all")

test_that("removing only a constant variable", {
  out1 <- sim2data(sim, to="start_stop", remove_vars="F")
  out2 <- sim2data(sim, to="long", remove_vars="F")
  out3 <- sim2data(sim, to="wide", remove_vars="F")

  expect_true(!"F" %in% colnames(out1))
  expect_true(!"F" %in% colnames(out2))
  expect_true(!"F" %in% colnames(out3))
})

test_that("removing other variables", {
  rm_vars <- c("F", "A", "E", "D")

  out1 <- sim2data(sim, to="start_stop", remove_vars=rm_vars)
  out2 <- sim2data(sim, to="long", remove_vars=rm_vars)
  out3 <- sim2data(sim, to="wide", remove_vars=rm_vars)

  expect_equal(colnames(out1), c(".id", "start", "stop", "B", "C"))
  expect_equal(colnames(out2), c(".id", ".time", "B", "C"))
  expect_true(all(colnames(out3)==".id" | startsWith(colnames(out3), "B_") |
                    startsWith(colnames(out3), "C_")))
})
