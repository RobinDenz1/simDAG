
test_that("general test case", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="poisson", parents="A", betas=0.2, intercept=1)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 205.07)
})

test_that("calling the function directly", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10)
  dat <- as.data.frame(sim_from_dag(dag=dag, n_sim=100))

  out <- node_poisson(data=dat, parents="A", betas=0.2, intercept=1)

  expect_equal(mean(out), 205.07)
})

test_that("using a formula", {

  set.seed(23426)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="poisson", formula=~A + I(A^2), betas=c(0.2, 0.01),
         intercept=1)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 9830064)
})
