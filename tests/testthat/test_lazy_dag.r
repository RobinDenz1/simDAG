
test_that("general test case", {

  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=param(p)) +
      node("Y", type="poisson", formula= ~ param(int) + A*param(beta))
  )

  dag1 <- set_lazy_dag(ldag, params=c(p=0.5, int=-2, beta=0.3))
  dag2 <- set_lazy_dag(ldag, params=c(p=0.3, int=-5, beta=1.3))

  data1 <- sim_from_dag(dag1, n_sim=10)
  data2 <- sim_from_dag(dag2, n_sim=10)

  expect_equal(nrow(data1), 10)
  expect_equal(nrow(data2), 10)
})

test_that("set_lazy_dag() and lazy_dag() in function", {

  gen_data <- function(n_sim, sd_A, beta_Y) {

    # define a lazy DAG
    ldag <- lazy_dag(
      empty_dag() +
        node("A", type="rnorm", mean=1, sd=param(SD)) +
        node("Y", type="gaussian", formula= ~ -2 + A*param(BETA), error=1)
    )

    # set it using parameters
    params <- c(SD=sd_A, BETA=beta_Y)
    dag <- set_lazy_dag(ldag, params=params)

    # generate data
    data <- sim_from_dag(dag, n_sim=n_sim)

    return(data)
  }

  test <- gen_data(n_sim=10, sd_A=2, beta_Y=0)

  expect_true(is.data.table(test))
})

test_that("set_lazy_dag() in function, lazy_dag() outside", {

  # define a lazy DAG
  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rnorm", mean=1, sd=param(SD)) +
      node("Y", type="gaussian", formula= ~ -2 + A*param(BETA), error=1)
  )

  gen_data <- function(ldag, n_sim, sd_A, beta_Y) {

    # set it using parameters
    params <- c(SD=sd_A, BETA=beta_Y)
    dag <- set_lazy_dag(ldag, params=params)

    # generate data
    data <- sim_from_dag(dag, n_sim=n_sim)

    return(data)
  }

  test <- gen_data(ldag=ldag, n_sim=10, sd_A=2, beta_Y=0)

  expect_true(is.data.table(test))
})

test_that("S3 print method general", {

  dag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=0.1) +
      node("B", type="rbernoulli", p=param(p2)) +
      node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
           intercept=-10, error=10) +
      node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
           intercept=-5)
  )
  expect_snapshot_output(print(dag))
})

test_that("S3 print method with node that is both time-dependent and not", {

  dag <- lazy_dag(
    empty_dag() +
      node("calories", type="rnorm", mean=param(mu), sd=150) +
      node_td("calories", type="gaussian",
              formula= ~ 1 + calories*1.1, error=1)
  )
  expect_snapshot(print(dag))
})

test_that("S3 summary method general", {

  dag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=0.1) +
      node("B", type="rbernoulli", p=0.2) +
      node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
           intercept=-10, error=param(error)) +
      node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
           intercept=-5)
  )
  expect_snapshot_output(summary(dag))
})

test_that("S3 summary method with dag filled with formulas", {

  dag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=0.1) +
      node("B", type="rbernoulli", p=0.2) +
      node("C", type="gaussian", formula= ~ -10 + A*0.1 + B*0.2, error=10) +
      node("D", type="binomial", formula= ~ -5 + B*7 + C*1) +
      node("E", type="poisson", formula= ~ -2 + A*4 + C*3 + A:B*2) +
      node("F", type="negative_binomial", formula= ~ param(intercept) + A*4 +
             C*3 + A:B*2, theta=param(theta)) +
      node("G", type="cox", formula= ~ A*4 + C*3 + A:B*2,
           surv_dist="weibull", lambda=10, gamma=0.23, cens_dist=NULL)
  )
  expect_snapshot_output(summary(dag))
})

test_that("S3 plot method general", {

  dag <- lazy_dag(
    empty_dag() +
      node("node 1", type="rnorm", mean=param(ahhh)) +
      node("A", type="rbernoulli") +
      node("B", type="gaussian", parents=c("node 1", "A")) +
      node("C", type="binomial", parents=c("node 1", "A", "B")) +
      node("D", type="multinomial", parents=c("node 1", "C"))
  )

  set.seed(1234)
  expect_snapshot_output(plot(dag))
})

test_that("error on no input", {
  expect_error(lazy_dag(), "Please supply a DAG expression.")
})

test_that("error without any param() calls", {
  expect_error({
    lazy_dag(
      empty_dag() +
        node("A", type="rnorm", mean=10, sd=4)
    )
  }, "No `param()` placeholders found in the DAG expression.", fixed=TRUE)
})

test_that("error with wrong inputs", {

  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rnorm", mean=10, sd=param(sd)) +
      node("Y", type="gaussian", formula= ~ -2 + A*param(beta))
  )

  # wrong lazy_dag
  expect_error(set_lazy_dag(10, params=c(sd=1, beta=2)),
               "`x` must be a `lazy_DAG` object.")

  # non-fitting names
  expect_error(set_lazy_dag(ldag, params=c(SD=1, beta=2)),
               "Missing parameter values: sd")

  # too many parameters
  expect_error(set_lazy_dag(ldag, params=c(sd=1, beta=2, beta2=3)),
               "Unused parameter values supplied: beta2")

  # NULL values not allowed
  expect_error(set_lazy_dag(ldag, params=list(sd=NULL, beta=2)),
               "Parameter values must not be NULL.")

  # wrong params input entirely
  expect_error(set_lazy_dag(ldag, 0),
               "`params` must be a named list or named atomic vector.")

})

test_that("wrong name in params()", {

  expect_error({
    ldag <- lazy_dag(
      empty_dag() +
        node("A", type="rbernoulli", p=param())
    )
  }, paste0("Use `param()` with exactly one unquoted parameter name, ",
             "for example: `param(intercept)` would be valid syntax."),
  fixed=TRUE
  )
})

test_that("lazy_DAGs cannot be used to simulate data", {

  # in sim_from_dag()
  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rnorm", mean=param(mu), sd=2)
  )

  expect_error(sim_from_dag(ldag, n_sim=100),
               paste0("'lazy_DAG' objects cannot be used directly to ",
               "simulate data. Please run set_lazy_dag() first, ",
               "see ?lazy_dag for more information."), fixed=TRUE)

  # in sim_discrete_time()
  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rnorm", mean=param(mu), sd=2) +
      node_td("Y", type="time_to_event", prob_fun=0.01)
  )

  expect_error(sim_discrete_time(ldag, n_sim=100, max_t=10),
               paste0("'lazy_DAG' objects cannot be used directly to ",
                      "simulate data. Please run set_lazy_dag() first, ",
                      "see ?lazy_dag for more information."), fixed=TRUE)

  # in sim_discrete_event()
  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rnorm", mean=param(mu), sd=2) +
      node_td("Y", type="next_time", prob_fun=0.01)
  )

  expect_error(sim_discrete_event(ldag, n_sim=100, max_t=10),
               paste0("'lazy_DAG' objects cannot be used directly to ",
                      "simulate data. Please run set_lazy_dag() first, ",
                      "see ?lazy_dag for more information."), fixed=TRUE)
})
