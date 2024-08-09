
test_that("sampling events", {

  set.seed(435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", parents="A", betas=1, intercept=-2)

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_true(sum(dat$B)==99)
})

test_that("calling the function directly", {

  set.seed(435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3)
  data <- as.data.frame(sim_from_dag(dag=dag, n_sim=100))

  out <- node_binomial(data=data, parents="A", betas=1, intercept=-2)

  expect_true(sum(out)==99)
})

test_that("getting probabilities", {

  set.seed(43565)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", parents="A", betas=1, intercept=-2,
         return_prob=TRUE)

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(mean(dat$B), 0.992167, tolerance=0.0001)
  expect_equal(stats::sd(dat$B), 0.02805198, tolerance=0.0001)
})

test_that("as factor", {

  set.seed(65346)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", parents="A", betas=1, intercept=-6,
         output="factor")

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(levels(dat$B), c("FALSE", "TRUE"))
})

test_that("as factor + labels", {

  set.seed(65346)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", parents="A", betas=1, intercept=-6,
         output="factor", labels=c("male", "female"))

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_equal(levels(dat$B), c("male", "female"))
})

test_that("as numeric", {

  set.seed(65456546)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", parents="A", betas=1, intercept=-6,
         output="numeric")

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_true(all(dat$B==0 | dat$B==1))
})

test_that("with formula", {

  set.seed(435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", formula=~ A, betas=1, intercept=-2)

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_true(sum(dat$B)==99)
})

test_that("with special formula", {

  set.seed(435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=3) +
    node("B", type="binomial", formula=~ -2 + A*1)

  dat <- sim_from_dag(dag=dag, n_sim=100)

  expect_true(sum(dat$B)==99)
})
