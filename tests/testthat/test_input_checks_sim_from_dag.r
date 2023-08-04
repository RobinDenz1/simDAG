
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

test_that("wrong dag", {
  expect_error(sim_from_dag(dag="", n_sim=10))
})

test_that("wrong n_sim", {
  expect_error(sim_from_dag(dag=dag, n_sim="10"))
})

test_that("wrong sort_dag", {
  expect_error(sim_from_dag(dag=dag, n_sim=10, sort_dag=2))
})

test_that("dag with node_td", {
  dag <- dag + node_td("testin", type="time_to_event")

  expect_error(sim_from_dag(dag, n_sim=10))
})
