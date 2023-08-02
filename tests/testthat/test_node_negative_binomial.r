
test_that("general test case", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="negative_binomial", parents="A", betas=0.2, theta=0.05,
         intercept=-2)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 8.02)
})

test_that("using a formula", {

  set.seed(243556336)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="negative_binomial", formula=~A + I(A^2),
         betas=c(0.2, 0.01), theta=0.05, intercept=-2)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 7164.28)
})
