
test_that("with one parent node", {

  set.seed(4526345)

  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         output="factor", probs=c(0.4, 0.6)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_distr", parents="sex", distr=distr)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_equal(mean(data$A[data$sex=="female"]), 1.649832, tolerance=0.001)
  expect_equal(sd(data$A[data$sex=="female"]), 0.6189326, tolerance=0.001)

  expect_equal(mean(data$A[data$sex=="male"]), 100.3328, tolerance=0.001)
  expect_equal(sd(data$A[data$sex=="male"]), 4.829392, tolerance=0.001)
})

test_that("with two parent nodes", {

  set.seed(45435)

  # define conditional distributions with interaction between parents
  distr <- list(male.FALSE=list("rnorm", mean=100, sd=5),
                male.TRUE=list("rnorm", mean=100, sd=20),
                female.FALSE=list("rbernoulli", p=0.5),
                female.TRUE=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

  # define DAG
  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         output="factor", probs=c(0.4, 0.6)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_distr", parents=c("sex", "chemo"), distr=distr)

  # generate data
  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_equal(mean(data$A), 37.42947, tolerance=0.001)
  expect_equal(sd(data$A), 48.41304, tolerance=0.001)
})

test_that("supplying data.frame & setting default_distr", {

  set.seed(3245)

  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

  data <- data.frame(sex=c("male", "male", "female", "trans"))

  out <- node_conditional_distr(data=data, parents="sex", distr=distr,
                                coerce2numeric=FALSE,
                                default_distr=runif)

  expect_true(is.numeric(out))
})
