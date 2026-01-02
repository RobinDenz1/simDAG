
test_that("general test case", {

  set.seed(1324)

  dag <- empty_dag() +
    node("A", type="runif") +
    node("B", type="rbernoulli") +
    node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05)

  # as two columns
  data <- sim_from_dag(dag, n_sim=1000)

  expect_equal(nrow(data), 1000)
  expect_equal(ncol(data), 4)
  expect_equal(round(mean(data$Y_time), 3), 6.704)
  expect_true(all(data$Y_status==1))

  # using only one column
  set.seed(1324)

  dag <- empty_dag() +
    node("A", type="runif") +
    node("B", type="rbernoulli") +
    node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05,
         as_two_cols=FALSE)

  data <- sim_from_dag(dag, n_sim=1000)

  expect_equal(nrow(data), 1000)
  expect_equal(ncol(data), 3)
  expect_equal(round(mean(data$Y), 3), 6.704)
})

test_that("with censoring", {

  set.seed(13244)

  dag <- empty_dag() +
    node("A", type="runif") +
    node(c("B", "C"), type="rbernoulli") +
    node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05 + C*0.05,
         cens_dist="runif", cens_args=list(min=5, max=10))

  data <- sim_from_dag(dag, n_sim=1000)

  expect_equal(nrow(data), 1000)
  expect_equal(ncol(data), 5)
  expect_equal(round(mean(data$Y_time), 3), 3.977)
  expect_true(!all(data$Y_status==1))
  expect_true(max(data$Y_time) <= 10)
})

test_that("with left truncation", {

  set.seed(13244)

  dag <- empty_dag() +
    node("A", type="runif") +
    node(c("B", "C"), type="rbernoulli") +
    node("Y", type="aalen", formula= ~ 0.1 + A*0.2 + B*-0.05 + C*0.05,
         as_two_cols=FALSE, left=5)

  data <- sim_from_dag(dag, n_sim=1000)

  expect_true(all(data$Y > 5))
})

test_that("error with negative hazard", {

  set.seed(13244)

  dag <- empty_dag() +
    node("A", type="runif") +
    node(c("B", "C"), type="rbernoulli") +
    node("Y", type="aalen", formula= ~ 0.1 + A*-0.2 + B*-0.05 + C*0.05,
         as_two_cols=FALSE, left=5)

  expect_error(sim_from_dag(dag, n_sim=1000))
})
