
test_that("general test case empty_dag()", {

  expected <- list(root_nodes=list(), child_nodes=list(),
                   tx_nodes=list(), networks=list(),
                   td_networks=list())
  class(expected) <- "DAG"

  out <- empty_dag()

  expect_equal(out, expected)
})

test_that("S3 print method with empty dag", {

  dag <- empty_dag()

  expect_snapshot_output(print(dag))
})

test_that("S3 print method with filled dag", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5)

  expect_snapshot_output(print(dag))
})

test_that("S3 print method with node that is both time-dependent and not", {

  dag <- empty_dag() +
    node("calories", type="rnorm", mean=2500, sd=150) +
    node_td("calories", type="gaussian",
            formula= ~ 1 + calories*1.1, error=1)

  expect_snapshot(print(dag))
})

test_that("S3 summary method with empty dag", {

  dag <- empty_dag()

  expect_snapshot_output(summary(dag))
})

test_that("S3 summary method with filled dag", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 0.2),
         intercept=-10, error=10) +
    node("D", type="binomial", parents=c("B", "C"), betas=c(7, 1),
         intercept=-5)

  expect_snapshot_output(summary(dag))
})

test_that("S3 summary method with dag filled with formulas", {

  dag <- empty_dag() +
    node("A", type="rbernoulli", p=0.1) +
    node("B", type="rbernoulli", p=0.2) +
    node("C", type="gaussian", formula= ~ -10 + A*0.1 + B*0.2, error=10) +
    node("D", type="binomial", formula= ~ -5 + B*7 + C*1) +
    node("E", type="poisson", formula= ~ -2 + A*4 + C*3 + A:B*2) +
    node("F", type="negative_binomial", formula= ~ -2 + A*4 + C*3 + A:B*2,
         theta=0.34) +
    node("G", type="cox", formula= ~ A*4 + C*3 + A:B*2,
         surv_dist="weibull", lambda=10, gamma=0.23, cens_dist=NULL)

  expect_snapshot_output(summary(dag))
})
