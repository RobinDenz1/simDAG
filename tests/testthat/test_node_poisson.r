
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

test_that("using a special formula", {

  set.seed(23426)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="poisson", formula=~1 + A*0.2 + I(A^2)*0.01,
         intercept=1)

  out <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(out$B), 9830064)
})

test_that("using different link functions", {

  set.seed(23426)

  ## link = "identity"
  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="poisson", formula=~1 + A*0.2 + I(A^2)*0.01,
         intercept=1, link="identity")
  out <- sim_from_dag(dag=dag, n_sim=100)
  expect_equal(round(mean(out$B), 3), 6.56)

  ## link = "inverse"
  dag <- empty_dag() +
    node("A", type="rnorm", mean=12, sd=10) +
    node("B", type="poisson", formula=~1 + A*0.2 + I(A^2)*0.01,
         intercept=1, link="sqrt")
  out <- sim_from_dag(dag=dag, n_sim=100)
  expect_equal(round(mean(out$B), 3), 53.78)
})

test_that("input checks with link argument", {

  # wrong input
  expect_error({
    dag <- empty_dag() +
      node("A", type="poisson", formula=~ -1 + A*-2 + BTRUE*11, error=1,
           link=c("something", "22"))
  })

  # unsupported link
  expect_error({
    dag <- empty_dag() +
      node("A", type="poisson", formula=~ -1 + A*-2 + BTRUE*11, error=1,
           link="logit")
  })
})
