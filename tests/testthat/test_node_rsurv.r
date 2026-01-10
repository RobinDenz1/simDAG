
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

test_that("general test case ehreg", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="ehreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2, phi=c(1, 2, 0.3))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 1.66)
})

test_that("general test case ypreg", {

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="ypreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2, phi=c(1, 2, 0.3))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y_time), 3), 1.57)
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

test_that("using left and right arguments", {

  # currently needs to be skipped, cause the required rsurv version is not
  # on CRAN yet
  skip_on_cran()
  skip_on_ci()

  set.seed(1234)

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2, as_two_cols=FALSE,
         left=34, right=Inf)
  data <- sim_from_dag(dag, n_sim=100)

  expect_true(all(data$Y > 34))

  dag <- empty_dag() +
    node(c("A", "B", "C"), type="rnorm", mean=0, sd=1) +
    node("Y", type="aftreg", formula= ~ -2 + A*0.2 + B*0.1 + A:B*1,
         baseline="weibull", scale=1, shape=2, as_two_cols=FALSE,
         left=0, right=1)
  data <- sim_from_dag(dag, n_sim=100)

  expect_true(all(data$Y < 1))
})
