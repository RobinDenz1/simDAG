
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

test_that("using it to get the linear predictor", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5, output="numeric") +
    node("C", type="rcategorical", probs=c(0.1, 0.3, 0.6),
         labels=c("high", "medium", "low")) +
    node("something", type="gaussian", formula= ~ -2 + age*3 + sex*0.3 +
           Cmedium*0.4 + Clow*9, error=0.1) +
    node("test1", type="identity",
         formula= ~ 1 + age*2 + sex*0.2 + age:sex*1.3, kind="linpred") +
    node("test2", type="gaussian",
         formula= ~ 1 + age*2 + sex*0.2 + age:sex*1.3, error=0)
  sim_dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(sim_dat$test1, sim_dat$test2)
})

test_that("using it to get the linear predictor in discrete-time simulation", {

  set.seed(345345)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5, output="numeric") +
    node("C", type="rcategorical", probs=c(0.1, 0.3, 0.6),
         labels=c("high", "medium", "low")) +
    node("something", type="gaussian", formula= ~ -2 + age*3 + sex*0.3 +
           Cmedium*0.4 + Clow*9, error=0.1) +
    node_td("D", type="time_to_event", prob_fun=0.1) +
    node_td("test1", type="identity",
         formula= ~ 1 + age*2 + sex*0.2 + age:sex*1.3 + D_eventTRUE*0.1,
         kind="linpred") +
    node_td("test2", type="gaussian",
         formula= ~ 1 + age*2 + sex*0.2 + age:sex*1.3 + D_eventTRUE*0.1, error=0)

  sim <- sim_discrete_time(dag, n_sim=100, max_t=5, save_states="all")
  data <- sim2data(sim, to="long")

  expect_equal(data$test1, data$test2)
})

test_that("returning just the data", {

  set.seed(32414)
  g <- igraph::sample_gnm(n=10, m=15)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5, output="numeric") +
    node("C", type="rcategorical", probs=c(0.1, 0.3, 0.6),
         labels=c("high", "medium", "low"))

  # just one variable
  set.seed(3455)
  dag2 <- dag + node("Y", type="identity", formula= ~ age:sex, kind="data")
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==4)
  expect_equal(colnames(data), c("age", "sex", "C", "Y"))
  expect_equal(round(mean(data$Y), 3), 24.691)

  # multiple variables
  set.seed(3455)
  dag2 <- dag + node("Y", type="identity", formula= ~ age:sex + Cmedium +
                       I(age^2), kind="data")
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==6)
  expect_equal(colnames(data), c("age", "sex", "C", "age:sex", "Cmedium",
                                 "I(age^2)"))
  expect_equal(round(mean(data$`age:sex`), 3), 24.691)

  # multiple variables + var_names
  set.seed(3455)
  dag2 <- dag + node("Y", type="identity", formula= ~ age:sex + Cmedium +
                       I(age^2), kind="data",
                     var_names=c("age_sex_interact", "c_medium", "age_squared"))
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==6)
  expect_equal(colnames(data), c("age", "sex", "C", "age_sex_interact",
                                 "c_medium", "age_squared"))
  expect_equal(round(mean(data$age_sex_interact), 3), 24.691)

  # a single net() call
  set.seed(3455)
  dag2 <- dag +
    network("network1", net=g) +
    node("Y", type="identity", formula= ~ net(mean(age)), kind="data")
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==4)
  expect_equal(colnames(data), c("age", "sex", "C", "Y"))
  expect_equal(round(mean(data$Y), 3), 50.522)

  # multiple net calls
  set.seed(3455)
  dag2 <- dag +
    network("network1", net=g) +
    node("Y", type="identity", formula= ~ net(mean(age)) + net(sum(age)) +
           net(mean(age), order=2),
         kind="data")
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==6)
  expect_equal(colnames(data), c("age", "sex", "C", "net(mean(age))",
                                 "net(sum(age))", "net(mean(age),order=2)"))
  expect_equal(round(mean(data$`net(mean(age))`), 3), 50.522)

  # mixing regular formula syntax and net() calls
  set.seed(3455)
  dag2 <- dag +
    network("network2", net=g) +
    node("Y", type="identity",
         formula= ~ age:sex + Cmedium + I(age^2) + net(sum(age)), kind="data")
  data <- sim_from_dag(dag2, n_sim=10)

  expect_true(ncol(data)==7)
  expect_equal(colnames(data), c("age", "sex", "C", "age:sex", "Cmedium",
                                 "I(age^2)", "net(sum(age))"))
  expect_equal(round(mean(data$`net(sum(age))`), 3), 150.292)

  # error with mixed model terms
  dag2 <- dag + node("Y", type="identity", formula= ~ I(age^2) + (1|D),
                     kind="data")
  expect_error(sim_from_dag(dag=dag2, n_sim=10))
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

test_that("error when used as root node", {

  expect_error({
    dag <- empty_dag() +
    node("A", type="identity", formula= ~ 20)},
  paste0("Nodes of type 'identity' cannot be used as root nodes, e.g. at",
         " least one variable name has to be\nmentioned in 'formula'. Use",
         " type='rconstant' instead to specify a constant value.")
  )
})
