
test_that("with censoring", {

  set.seed(3245)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5) +
    node("C", type="cox", parents=c("A", "B"), betas=c(0.2, 1),
         lambda=2, gamma=1, surv_dist="weibull", cens_dist="runif",
         cens_args=list(min=0, max=10000))

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(colnames(out), c("A", "B", "C_time", "C_status"))
  expect_true(all(out$C_status==1))
  expect_equal(mean(out$C_time), 0.04942, tolerance=0.0001)
})

test_that("calling the function directly", {

  set.seed(3245)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5)
  data <- as.data.frame(sim_from_dag(dag=dag, n_sim=100))

  out <- node_cox(data=data, parents=c("A", "B"), betas=c(0.2, 1),
                  lambda=2, gamma=1, surv_dist="weibull", cens_dist="runif",
                  cens_args=list(min=0, max=10000), name="C")

  expect_equal(colnames(out), c("C_time", "C_status"))
  expect_true(all(out$C_status==1))
  expect_equal(mean(out$C_time), 0.04942, tolerance=0.0001)
})

test_that("without censoring", {

  set.seed(324545)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5) +
    node("C", type="cox", parents=c("A", "B"), betas=c(0.2, 1),
         lambda=2, gamma=1, surv_dist="weibull", cens_dist=NULL)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(colnames(out), c("A", "B", "C_time", "C_status"))
  expect_true(all(out$C_status==1))
  expect_equal(mean(out$C_time), 0.04249061, tolerance=0.0001)
})

test_that("with formula", {

  set.seed(3245)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5) +
    node("C", type="cox", formula=~A + B, betas=c(0.2, 1),
         lambda=2, gamma=1, surv_dist="weibull", cens_dist="runif",
         cens_args=list(min=0, max=10000))

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(colnames(out), c("A", "B", "C_time", "C_status"))
  expect_true(all(out$C_status==1))
  expect_equal(mean(out$C_time), 0.04942, tolerance=0.0001)
})

test_that("with enhanced formula", {

  set.seed(3245)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5) +
    node("C", type="cox", formula=~A*0.2 + BTRUE*1,
         lambda=2, gamma=1, surv_dist="weibull", cens_dist="runif",
         cens_args=list(min=0, max=10000))

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(colnames(out), c("A", "B", "C_time", "C_status"))
  expect_true(all(out$C_status==1))
  expect_equal(mean(out$C_time), 0.04942, tolerance=0.0001)
})

test_that("without censoring: exponential", {

  set.seed(324545)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=2) +
    node("B", type="rbernoulli", p=0.5) +
    node("C", type="cox", parents=c("A", "B"), betas=c(0.2, 1),
         lambda=2, gamma=1, surv_dist="exponential", cens_dist=NULL)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(colnames(out), c("A", "B", "C_time", "C_status"))
})
