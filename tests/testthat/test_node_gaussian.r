
test_that("general test case", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", parents=c("A", "B"), betas=c(10, 11),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 101.96705, tolerance=0.001)
})

test_that("calling the function directly", {

  set.seed(43525)

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1)
  data <- as.data.frame(sim_from_dag(dag, n_sim=1000))

  out <- node_gaussian(data=data, parents=c("A", "B"), betas=c(10, 11),
                       intercept=-100, error=10)

  expect_equal(mean(out), 101.96705, tolerance=0.001)
})

test_that("with formula", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", formula=~ A + B, betas=c(10, 11),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 101.96705, tolerance=0.001)
})

test_that("with formula + quadratic effects", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=0, sd=1) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ A + I(A^2) + B,
         betas=c(0.2, 0.1, 0.5),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -99.67325, tolerance=0.001)
})

test_that("with formula + categorical independent variable", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rcategorical", probs=c(0.1, 0.3, 0.6), coerce2factor=TRUE) +
    node("gauss", type="gaussian", formula=~ A + B, betas=c(10, 11, -10),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 99.10283, tolerance=0.001)
})

test_that("with formula + interaction term", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ A + B + A*B, betas=c(10, 11, -20),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -83.31415, tolerance=0.001)
})
