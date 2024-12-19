
test_that("general test", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("C", type="rcategorical", probs=c(0.1, 0.3, 0.6),
         labels=c("high", "medium", "low")) +
    node("something", type="gaussian", formula= ~ -2 + age*3 + sexTRUE*0.3 +
           Cmedium*0.4 + Clow*9, error=0.1) +
    node("test1", type=node_identity,
         formula= ~ age / 2 - sex + something^2 -
           fifelse(C=="high", 3, 4) + rnorm(n=1000))
  sim_dat <- sim_from_dag(dag, n_sim=1000)

  expect_true(is.numeric(sim_dat$test1))
})

test_that("using custom function in formula", {

  # simple return 1 n times
  custom_fun <- function(n) {
    rep(1, n)
  }

  # function needs to be global
  assign("custom_fun", value=custom_fun, envir=.GlobalEnv)

  dag <- empty_dag() +
    node("A", type="rnorm") +
    node("B", type="identity", formula= ~ A + custom_fun(n=100))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(data$B, data$A + 1)
})

test_that("as child node, parents specified", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("bmi", type="gaussian", parents=c("sex", "age"),
         betas=c(1.1, 0.4), intercept=12, error=2) +
    node("combine", type="identity", parents=c("age", "sex", "bmi"),
         formula= ~ age * 0.5 + ifelse(sex, 10, -10) - bmi)

  sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
  sim_dat[, combine2 := age * 0.5 + ifelse(sex, 10, -10) - bmi]

  expect_equal(sim_dat$combine, sim_dat$combine2)
})

test_that("as child node, parents not specified", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("bmi", type="gaussian", parents=c("sex", "age"),
         betas=c(1.1, 0.4), intercept=12, error=2) +
    node("combine", type="identity",
         formula= ~ age * 0.5 + ifelse(sex, 10, -10) - bmi)

  sim_dat <- sim_from_dag(dag=dag, n_sim=1000)
  sim_dat[, combine2 := age * 0.5 + ifelse(sex, 10, -10) - bmi]

  expect_equal(sim_dat$combine, sim_dat$combine2)
})

test_that("as time-varying node, parents specified", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("bmi", type="gaussian", parents=c("sex", "age"),
         betas=c(1.1, 0.4), intercept=12, error=2) +
    node_td("death", type="time_to_event", prob_fun=0.4) +
    node_td("combine", type="identity",
            parents=c("age", "sex", "bmi", "death_event"),
            formula= ~ age * 0.5 + ifelse(sex, 10, -10) - bmi + death_event)

  sim_dat <- sim_discrete_time(dag, n_sim=100, max_t=10)$data
  sim_dat[, combine2 := age * 0.5 + ifelse(sex, 10, -10) - bmi + death_event]

  expect_equal(sim_dat$combine, sim_dat$combine2)
})

test_that("as time-varying node, parents not specified", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("bmi", type="gaussian", parents=c("sex", "age"),
         betas=c(1.1, 0.4), intercept=12, error=2) +
    node_td("death", type="time_to_event", prob_fun=0.4) +
    node_td("combine", type="identity",
            formula= ~ age * 0.5 + ifelse(sex, 10, -10) - bmi + death_event)

  sim_dat <- sim_discrete_time(dag, n_sim=100, max_t=10)$data
  sim_dat[, combine2 := age * 0.5 + ifelse(sex, 10, -10) - bmi + death_event]

  expect_equal(sim_dat$combine, sim_dat$combine2)
})
