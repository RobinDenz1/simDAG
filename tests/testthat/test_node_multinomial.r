
test_that("general test case", {

  set.seed(3345235)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5) +
    node("UICC", type="multinomial", parents=c("sex", "age"),
         betas=matrix(c(0.2, 0.4, 0.1, 0.5, 1.1, 1.2), ncol=2),
         intercepts=1)

  sim_dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(as.vector(table(sim_dat$UICC)), c(1, 99))
})

test_that("calling the function directly", {

  set.seed(3345235)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5)
  sim_dat <- as.data.frame(sim_from_dag(dag=dag, n_sim=100))

  out <- node_multinomial(data=sim_dat, parents=c("sex", "age"),
         betas=matrix(c(0.2, 0.4, 0.1, 0.5, 1.1, 1.2), ncol=2),
         intercepts=1)

  expect_equal(as.vector(table(out)), c(1, 99))
})

test_that("returning probabilities", {

  set.seed(3345235)

  dag <- empty_dag() +
    node("age", type="rnorm", mean=50, sd=4) +
    node("sex", type="rbernoulli", p=0.5)

  dat <- sim_from_dag(dag, n_sim=50)

  probs <- node_multinomial(data=dat,
                            parents=c("sex", "age"),
                            betas=matrix(c(0.2, 0.4, 0.1, 0.5, 1.1, 1.2),
                                         ncol=2),
                            intercepts=1,
                            return_prob=TRUE)

  expect_true(is.matrix(probs))
  expect_true(is.numeric(probs))
})
