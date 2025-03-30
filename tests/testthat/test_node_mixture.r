
test_that("binary condition, linear outcome", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("Y", type="mixture", parents="A",
         distr=list(
           "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
           "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1)
         ))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.722)
})

test_that("multi-variable conditions, linear outcome", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("C", type="rnorm") +
    node("Y", type="mixture", parents=c("A", "C"),
         distr=list(
           "A==0 & C > 0",
           node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
           "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1)
         ))

  data <- sim_from_dag(dag, n_sim=100)
  expect_true(all(is.na(data$Y[data$A==0 & data$C <= 0])))
  expect_equal(round(mean(data$Y, na.rm=TRUE), 3), 1.521)
})

test_that("categorical condition, linear outcome", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rcategorical", probs=c(0.333, 0.333, 0.333)) +
    node("B", type="rnorm") +
    node("Y", type="mixture", parents="A",
         distr=list(
           "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
           "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1),
           "A==2", node(".", type="gaussian", formula= ~ 15 + B*1, error=1)
         ))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 6.093)
})

test_that("binary condition, binary outcome", {

  set.seed(1535434)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("Y", type="mixture", parents="A",
         distr=list(
           "A==0", node(".", type="binomial", formula= ~ -2 + B*2),
           "A==1", node(".", type="binomial", formula= ~ 3 + B*5)
         ))

  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.38)
})

test_that("conditions not covering all scenarios", {

  set.seed(1535434)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("Y", type="mixture", parents="A",
         distr=list(
           "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1)
         ))

  data <- sim_from_dag(dag, n_sim=100)
  expect_true(all(is.na(data$Y[data$A==1])))
  expect_equal(round(mean(data$Y[data$A==0]), 3), -2.711)
})

test_that("conditions not covering all scenarios, using default", {

  set.seed(1535434)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm") +
    node("Y", type="mixture", parents="A",
         distr=list(
           "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1)
         ), default=10)

  data <- sim_from_dag(dag, n_sim=100)
  expect_true(all(data$Y[data$A==1]==10))
  expect_equal(round(mean(data$Y[data$A==0]), 3), -2.711)
})

test_that("calling the function directly", {

  set.seed(1535434)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rnorm")
  data <- as.data.frame(sim_from_dag(dag, n_sim=100))

  out <- node_mixture("Y", data=data, parents="A",
       distr=list(
         "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1)
       ), default=10)

  expect_true(all(out[data$A==1]==10))
  expect_equal(round(mean(out[data$A==0]), 3), -2.711)
})

test_that("nothing in distr defined", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm") +
      node("B", type="mixture", parents="A", distr=list())
  }, "'distr' must contain at least two entries.")
})

test_that("wrong condition / node order", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rbernoulli") +
      node("B", type="rnorm") +
      node("B", type="mixture", parents="A", distr=list(
        node(".", type="gaussian", formula= ~ -2 + B*2, error=1), "A==0"
      ))
  }, paste0("'distr' must contain single character strings containing the ",
            "condition for the corresponding node() definition first, ",
            "then the node() objects."), fixed=TRUE)
})

test_that("wrong number of entries in distr", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm") +
      node("B", type="mixture", parents="A", distr=list(
        "a", "b", "c"
      ))
  }, paste0("'distr' must contain an even number of entries, with the ",
            "condition first and the node second."))
})

test_that("right condition but wrong node part", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rbernoulli") +
      node("B", type="rnorm") +
      node("B", type="mixture", parents="A", distr=list(
        "A==0", "rnorm"
      ))
  }, paste0("'distr' must contain DAG.node objects created using the ",
            "node() function for the corresponding conditions only",
            " after the conditions."), fixed=TRUE)
})

test_that("with time-dependent node in distr", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rbernoulli") +
      node("B", type="rnorm") +
      node("B", type="mixture", parents="A", distr=list(
        "A==0", node_td(".", type="gaussian", formula= ~ -2 + B*2, error=1)
      ))
  }, paste0("Time-dependent nodes defined with the node_td() function are ",
            "currently not supported in 'distr'."), fixed=TRUE)
})

test_that("default not a single value", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rbernoulli") +
      node("B", type="rnorm") +
      node("Y", type="mixture", parents="A",
           distr=list(
             "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
             "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1)
           ), default=c(0, 1))
  }, "'default' should be a single value of some kind.")
})
