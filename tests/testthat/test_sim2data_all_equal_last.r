
## generating example data
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash_event, 0.1, 0.0001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex", event_duration=10, immunity_duration=12) +
  node_td("death", type="time_to_event", prob_fun=prob_death,
          parents="car_crash_event", event_duration=Inf)

set.seed(455)
sim <- sim_discrete_time(dag, n_sim=100, max_t=500, save_states="all")

test_that("start-stop: .all equal to .last", {

  d_all <- sim2data(sim, to="start_stop", use_saved_states=TRUE)
  d_last <- sim2data(sim, to="start_stop", use_saved_states=FALSE)

  expect_equal(d_all, d_last)
})

test_that("long: .all equal to .last", {

  d_all <- sim2data(sim, to="long", use_saved_states=TRUE)
  d_last <- sim2data(sim, to="long", use_saved_states=FALSE)
  setcolorder(d_last, c(".id", ".time", "sex", "car_crash", "death"))

  expect_equal(d_all, d_last)
})
