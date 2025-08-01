
set.seed(23523)

# a function to calculate the probability of death as a
# linear combination of age, sex and bmi on the log scale
prob_death <- function(data, beta_age, beta_sex, beta_bmi, intercept) {
  prob <- intercept + data$age*beta_age + data$sex*beta_sex + data$bmi*beta_bmi
  prob <- 1/(1 + exp(-prob))
  return(prob)
}

# nodes for generating data at t0
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2) +
  node_td("death", type="time_to_event", parents=c("age", "sex", "bmi"),
          prob_fun=prob_death, beta_age=0.1, beta_bmi=0.3,
          beta_sex=-0.2, intercept=-20, event_duration=Inf,
          save_past_events=TRUE)

# run simulation for 100 people, 50 days long
sim <- sim_discrete_time(n_sim=100,
                         dag=dag,
                         max_t=50,
                         verbose=FALSE)

test_that("as start_stop data", {

  out <- sim2data(sim=sim, to="start_stop")
  out2 <- as.data.table(sim, to="start_stop")

  expect_equal(out, out2)
  expect_true(inherits(out, "data.table"))
  expect_true(nrow(out)==119)
  expect_equal(colnames(out), c(".id", "start", "stop", "death", "age",
                                "sex", "bmi"))
})

test_that("as long data", {

  out <- sim2data(sim=sim, to="long")
  out2 <- as.data.table(sim, to="long")

  expect_true(inherits(out, "data.table"))
  expect_true(nrow(out)==5000)
  expect_equal(colnames(out), c(".id", ".time", "death", "age",
                                "sex", "bmi"))
})

test_that("as wide data", {

  out <- sim2data(sim=sim, to="wide")
  out2 <- as.data.table(out, to="wide")

  expect_true(inherits(out, "data.table"))
  expect_true(nrow(out)==100)
  expect_true(ncol(out)==54)
  expect_true(!"1" %in% colnames(out) & !"death_1" %in% colnames(out))
})

test_that("as start_stop data, with data.frame output", {

  out <- sim2data(sim=sim, to="start_stop", as_data_frame=TRUE)
  out2 <- as.data.frame(sim, to="start_stop")

  expect_true(inherits(out, "data.frame"))
  expect_true(nrow(out)==119)
  expect_equal(colnames(out), c(".id", "start", "stop", "death", "age",
                                "sex", "bmi"))
})

test_that("as wide data, save_states='all'", {

  set.seed(2343)

  sim <- sim_discrete_time(n_sim=100,
                           dag=dag,
                           max_t=50,
                           verbose=FALSE,
                           save_states="all")

  out <- sim2data(sim=sim, to="wide")

  expect_true(inherits(out, "data.table"))
  expect_true(nrow(out)==100)
  expect_true(ncol(out)==54)
})

test_that("as wide data, with multiple time-varying variables", {
  dag <- dag + node_td("More", type="time_to_event", prob_fun=0.01)
  sim <- sim_discrete_time(dag, n_sim=100, max_t=50)
  dat <- sim2data(sim, to="wide")

  expect_true("death_1" %in% colnames(dat))
})

test_that("wrong input to 'remove_vars'", {
  expect_error({sim2data(sim, to="start_stop", remove_vars=2)})
})
