
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=17, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.7),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="gaussian",
                         name="bmi",
                         betas=c(2.1, 1.4),
                         intercept=14,
                         error=2))

test_that("correct nrow, ncol", {
  sim_dat <- sim_from_dag(n_sim=55, root_nodes=root_nodes,
                          child_nodes=child_nodes)
  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat)==55)
  expect_true(ncol(sim_dat)==3)
})

test_that("snapshot test", {
  set.seed(42)
  sim_dat <- sim_from_dag(n_sim=100, root_nodes=root_nodes,
                          child_nodes=child_nodes)
  mod_bmi <- lm(bmi ~ sex + age, data=sim_dat)
  expect_equal(as.vector(round(mod_bmi$coefficients, 3)),
               c(13.707, 1.639, 1.434))
})

test_that("sort_dag working", {
  child_nodes <- list(list(parents=c("sex", "age", "income"),
                           type="gaussian",
                           name="bmi",
                           betas=c(2.1, 1.4, 0.1),
                           intercept=14,
                           error=2),
                      list(parents=c("sex", "age"),
                           type="gaussian",
                           name="income",
                           betas=c(0.1, 0.7),
                           intercept=100,
                           error=10))
  sim_dat <- sim_from_dag(n_sim=20, root_nodes=root_nodes,
                          child_nodes=child_nodes, sort_dag=TRUE)
  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat)==20)
  expect_true(ncol(sim_dat)==4)
  expect_error(sim_from_dag(n_sim=20, root_nodes=root_nodes,
                            child_nodes=child_nodes, sort_dag=FALSE))
})
