
test_that("general test case", {

  set.seed(457456)

  # get some example data from a known DAG
  dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex"), betas=c(1, 2),
         intercept=-10) +
    node("age", type="rnorm", mean=10, sd=2) +
    node("sex", parents="", type="rbernoulli", p=0.5) +
    node("smoking", parents=c("sex", "age"), type="binomial",
         betas=c(0.6, 0.2), intercept=-2)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  # suppose we only know the causal structure and the node type:
  dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex")) +
    node("age", type="rnorm") +
    node("sex", type="rbernoulli") +
    node("smoking", type="binomial", parents=c("sex", "age"))

  # get parameter estimates from data
  dag_full <- dag_from_data(dag=dag, data=data)


  expected_dag <- empty_dag() +
    node("death", type="binomial", parents=c("age", "sex"),
         betas=c(0.9699486, 1.8823713), intercept=-9.631358) +
    node("age", type="rnorm", mean=9.973241, sd=1.985875) +
    node("sex", type="rbernoulli", p=0.49) +
    node("smoking", type="binomial", parents=c("sex", "age"),
         betas=c(0.4799525, 0.2378301), intercept=-2.492591)

  expect_equal(dag_full$dag, expected_dag, tolerance=0.0001)

})

test_that("gen_node_rnorm", {

  data <- data.table::data.table(y=c(1, 2, 3, 4, 5))

  expected <- list(name="y",
                   type="rnorm",
                   parents=NULL,
                   params=list(mean=3,
                               sd=1.581139))

  out <- gen_node_rnorm(data=data, name="y", na.rm=TRUE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_rbernoulli", {

  data <- data.table::data.table(y=c(0, 0, 1, 0, 0))

  expected <- list(name="y",
                   type="rbernoulli",
                   parents=NULL,
                   params=list(p=0.2))

  out <- gen_node_rbernoulli(data=data, name="y", na.rm=TRUE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_rcategorical", {

  data <- data.table::data.table(y=c("0", "2", "1", "2", "0"))

  expected <- list(name="y",
                   type="rcategorical",
                   parents=NULL,
                   params=list(labels=c("0", "1", "2"), probs=c(0.4, 0.2, 0.4)))

  out <- gen_node_rcategorical(data=data, name="y", na.rm=TRUE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_conditional_prob", {

  data <- data.table::data.table(x=c("A", "A", "A", "B", "B", "B"),
                                 y=c(1, 1, 0, 0, 0, 0))

  expected <- list(name="y",
                   type="conditional_prob",
                   parents="x",
                   probs=list(A=2/3, B=0))

  out <- gen_node_conditional_prob(data=data, name="y", parents="x", na.rm=TRUE,
                                   return_model=TRUE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_gaussian", {

  data <- data.table::data.table(x=c(1, 2, 3, 4, 5),
                                 y=c(3, 4, 5, 6, 10))

  expected <- list(name="y",
                   type="gaussian",
                   parents="x",
                   betas=1.6,
                   intercept=0.8,
                   error=0.9486833)

  out <- gen_node_gaussian(name="y", parents="x", data=data, na.rm=TRUE,
                           return_model=FALSE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_binomial", {

  data <- data.table::data.table(x=c(1, 2, 3, 4, 5),
                                 y=c(0, 0, 1, 0, 1))

  expected <- list(name="y",
                   type="binomial",
                   parents="x",
                   betas=1.090426,
                   intercept=-3.893967)

  out <- gen_node_binomial(name="y", parents="x", data=data, na.rm=TRUE,
                           return_model=FALSE)

  expect_equal(out, expected, tolerance=0.0001)
})

test_that("gen_node_poisson", {

  data <- data.table::data.table(x=c(1, 2, 3, 4, 5),
                                 y=c(3, 4, 5, 6, 10))

  expected <- list(name="y",
                   type="poisson",
                   parents="x",
                   betas=0.2964423,
                   intercept=0.7471786)

  out <- gen_node_poisson(name="y", parents="x", data=data, na.rm=TRUE,
                           return_model=FALSE)

  expect_equal(out, expected, tolerance=0.0001)
})
