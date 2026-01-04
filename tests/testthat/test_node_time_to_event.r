set.seed(42)

dt <- data.table::data.table(.id=seq(1, 200),
                             "age"=rnorm(n=200, mean=30, sd=7.5),
                             "sex"=rbinom(n=200, size=1, prob=0.7))

dt$smoking <- ifelse(dt$sex == 0,
                     rbinom(n=nrow(dt[dt$sex == 0,]), size=1, prob=0.26),
                     rbinom(n=nrow(dt[dt$sex == 1,]), size=1, prob=0.20))
dt$sickness_event <- FALSE
dt$sickness_time <- NA_integer_
dt$sickness_past_event_times <- NA_integer_

prob_sick <- function(data, sim_time, rr_smoke0, rr_smoke1=5) {
  # smoking-dependent risk
  risk <- fifelse(data$smoking == 1, rr_smoke0, rr_smoke1)

  # sex- and age-dependet baseline risk
  base_p <- rep(0.1, nrow(data))

  base_p[data$sex == 0  & data$age < 18] <- 0
  base_p[data$sex == 0  & data$age >= 18 & data$age < 44] <- 0.004
  base_p[data$sex == 0  & data$age >= 45 & data$age < 54] <- 0.034
  base_p[data$sex == 0  & data$age >= 55 & data$age < 64] <- 0.077
  base_p[data$sex == 0  & data$age >= 65 & data$age < 74] <- 0.130
  base_p[data$sex == 0  & data$age >= 75] <- 0.241

  base_p[data$sex == 1  & data$age < 18] <- 0
  base_p[data$sex == 1  & data$age >= 18 & data$age < 44] <- 0.002
  base_p[data$sex == 1  & data$age >= 45 & data$age < 54] <- 0.009
  base_p[data$sex == 1  & data$age >= 55 & data$age < 64] <- 0.034
  base_p[data$sex == 1  & data$age >= 65 & data$age < 74] <- 0.071
  base_p[data$sex == 1  & data$age >= 75] <- 0.160

  p <- base_p * risk

  return(p)
}

test_that("correct nrow, ncol", {
  out <- node_time_to_event(dt,
                            parents=c("age", "sex", "smoking"),
                            sim_time=100,
                            name="sickness",
                            prob_fun=prob_sick,
                            rr_smoke0=1,
                            event_duration=1,
                            immunity_duration=100,
                            save_past_events=FALSE,
                            envir=NULL)

  expect_true(data.table::is.data.table(out))
  expect_true(nrow(out) == 200)
  expect_true(ncol(out) == 2)
  expect_equal(colnames(out), c("sickness_event", "sickness_time"))
})

test_that("correct sim_time", {
  out <- node_time_to_event(dt,
                            parents=c("age", "sex", "smoking"),
                            sim_time=50,
                            name="sickness",
                            prob_fun=prob_sick,
                            event_duration=1,
                            immunity_duration=100,
                            save_past_events=FALSE,
                            envir=NULL,
                            rr_smoke0=1,
                            rr_smoke1=5)

  expect_true(unique(out[sickness_event == TRUE]$sickness_time) == 50)
})

test_that("save_past_events working", {

  # setup that would usually happen in sim_discrete_time
  envir <- environment()
  past_events_list <- list(
    sickness_past_event_times=vector(mode="list", length=100))

  # calling the function once
  out1 <- node_time_to_event(dt,
                             parents=c("age", "sex", "smoking"),
                             sim_time=100,
                             name="sickness",
                             prob_fun=prob_sick,
                             rr_smoke0=10,
                             rr_smoke1=15,
                             event_duration=1,
                             immunity_duration=100,
                             save_past_events=TRUE,
                             envir=envir)
  expect_true(!is.null(past_events_list$sickness[[100]]))
  expect_true(is.null(past_events_list$sickness[[1]]))
})

test_that("time_since_last working", {
  set.seed(3245354)

  dag <- empty_dag() +
    node_td("sick", type="time_to_event", prob_fun=0.01,
            event_duration=3, time_since_last=TRUE)

  sim <- sim_discrete_time(dag, n_sim=10, max_t=100)

  expect_equal(colnames(sim$data), c(".id", "sick_event", "sick_time",
                                     "sick_time_since_last"))
  expect_true(is.numeric(sim$data$sick_time_since_last))
})

test_that("event_count working", {
  set.seed(3245354)

  dag <- empty_dag() +
    node_td("sick", type="time_to_event", prob_fun=0.1,
            event_duration=3, event_count=TRUE)

  sim <- sim_discrete_time(dag, n_sim=10, max_t=100)

  expect_equal(colnames(sim$data), c(".id", "sick_event", "sick_time",
                                     "sick_event_count"))
  expect_true(is.numeric(sim$data$sick_event_count))
})

test_that("using past_states in prob_fun", {
  set.seed(3245354)

  prob_sick <- function(data, past_states) {
    return(0.01)
  }

  dag <- empty_dag() +
    node_td("sick", type="time_to_event", prob_fun=prob_sick,
            event_duration=3)

  sim <- sim_discrete_time(dag, n_sim=10, max_t=100, save_states="all")

  expect_equal(colnames(sim$data), c(".id", "sick_event", "sick_time"))
})

test_that("using a custom root node without data argument", {

  set.seed(12544)

  dag <- empty_dag() +
    node_td("testing1", type=runif) +
    node_td("testing2", type=rbernoulli) +
    node_td("testing3", type=rnorm)
  expect_no_error({sim <- sim_discrete_time(dag, n_sim=10, max_t=20)})
})

test_that("using the 'unif' argument", {

  set.seed(1234)

  # this should be all FALSE, because unif < (1 - prob_fun)
  dag <- empty_dag() +
    node_td("test", type="time_to_event", prob_fun=0.1, unif=0.01)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=100)
  data <- sim2data(sim, to="start_stop")

  expect_equal(sum(data$test), 0)

  # this should be all TRUE, because unif >= (1 - prob_fun)
  dag <- empty_dag() +
    node_td("test", type="time_to_event", prob_fun=0.1, unif=0.999)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=100)
  data <- sim2data(sim, to="start_stop")

  expect_true(all(data$test))

  # this should be FALSE, TRUE, FALSE, TRUE, FALSE, ...
  dag <- empty_dag() +
    node_td("test", type="time_to_event", prob_fun=0.1,
            unif=rep(c(0.01, 0.999), 50))

  sim <- sim_discrete_time(dag, n_sim=100, max_t=100)
  data <- sim2data(sim, to="start_stop")
  data <- data[, .(any_TRUE = any(test)), by=.id]

  expect_equal(data$any_TRUE, rep(c(FALSE, TRUE), 50))

  # A and B should be the same
  dag <- empty_dag() +
    node_td("unif_A_B", type=runif) +
    node_td("A", type="time_to_event", prob_fun=0.1, unif="unif_A_B",
            parents="unif_A_B") +
    node_td("B", type="time_to_event", prob_fun=0.1, unif="unif_A_B")

  sim <- sim_discrete_time(dag, n_sim=100, max_t=100, save_states="all")
  data <- sim2data(sim, to="long")

  expect_equal(data$A, data$B)
})

test_that("using the formula argument is equal to calling node_binomial", {

  # using prob_fun
  prob_Y <- function(data) {
    out <- node_binomial(data=data, parents=c("A", "B"), betas=c(0.2, -0.2),
                         intercept=-2, return_prob=TRUE)
    return(out)
  }

  dag1 <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node_td("Y", type="time_to_event", prob_fun=prob_Y, parents=c("A", "B"))

  # using formula
  dag2 <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node_td("Y", type="time_to_event", formula= ~ -2 + A*0.2 + B*-0.2)

  set.seed(1234)
  sim1 <- sim_discrete_time(dag1, n_sim=100, max_t=5)

  set.seed(1234)
  sim2 <- sim_discrete_time(dag2, n_sim=100, max_t=5)

  expect_equal(sim1$data, sim2$data)
})

test_that("formula works with other links", {

  dag1 <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node_td("Y", type="time_to_event", formula= ~ -2 + A*1.2 + B*-0.2)

  dag2 <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node_td("Y", type="time_to_event", formula= ~ -2 + A*1.2 + B*-0.2,
            link="log")

  set.seed(1234)
  sim1 <- sim_discrete_time(dag1, n_sim=100, max_t=5)

  set.seed(1234)
  sim2 <- sim_discrete_time(dag2, n_sim=100, max_t=5)

  expect_true(sum(sim1$data$Y_event) != sum(sim2$data$Y_event))
})
