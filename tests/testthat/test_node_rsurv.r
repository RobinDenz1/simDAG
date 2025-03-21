
test_that("general test case aftreg", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2)

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 1.071)
})

test_that("general test case ahreg", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="ahreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2)

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 0.873)
})

test_that("general test case poreg", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="poreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2)

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 0.899)
})

test_that("just as one column", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2, as_two_cols=FALSE)

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 1.071)
})

test_that("with censoring", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2,
         cens_dist="rweibull", cens_args=list(scale=2, shape=1))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 0.671)
  expect_equal(sum(data$Y_status), 66)
})
