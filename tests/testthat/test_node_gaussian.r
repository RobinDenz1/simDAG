
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
