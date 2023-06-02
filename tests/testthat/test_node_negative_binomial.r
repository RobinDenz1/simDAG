
test_that("general test case", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="negative_binomial", parents="A", betas=0.2, theta=1)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 62.8)
})
