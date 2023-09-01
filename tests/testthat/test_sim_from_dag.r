
root_nodes <- list(list(type="rnorm",
                        params=list(mean=17, sd=4),
                        name="age",
                        time_varying=FALSE),
                   list(type="rbernoulli",
                        params=list(p=0.7),
                        name="sex",
                        time_varying=FALSE))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="gaussian",
                         name="bmi",
                         betas=c(2.1, 1.4),
                         intercept=14,
                         error=2,
                         time_varying=FALSE))
dag <- list(root_nodes=root_nodes,
            child_nodes=child_nodes,
            tx_nodes=list())
class(dag) <- "DAG"

test_that("correct nrow, ncol", {
  sim_dat <- sim_from_dag(dag=dag, n_sim=55)
  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat)==55)
  expect_true(ncol(sim_dat)==3)
})

test_that("snapshot test", {
  set.seed(42)
  sim_dat <- sim_from_dag(n_sim=100, dag=dag)
  mod_bmi <- lm(bmi ~ sex + age, data=sim_dat)
  expect_equal(as.vector(round(mod_bmi$coefficients, 3)),
               c(13.707, 1.639, 1.434))
})

test_that("sort_dag working", {
  child_nodes <- list(list(parents=c("sex", "age", "income"),
                           type="gaussian",
                           name="bmi",
                           betas=c(2.1, 1.4, 0.1),
                           intercept=14,
                           error=2,
                           time_varying=FALSE),
                      list(parents=c("sex", "age"),
                           type="gaussian",
                           name="income",
                           betas=c(0.1, 0.7),
                           intercept=100,
                           error=10,
                           time_varying=FALSE))
  dag$child_nodes <- child_nodes

  sim_dat <- sim_from_dag(n_sim=20, dag=dag, sort_dag=TRUE)
  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat)==20)
  expect_true(ncol(sim_dat)==4)
  expect_error(sim_from_dag(n_sim=20, dag=dag, sort_dag=FALSE))
})

test_that("helpful error message root nodes", {

  rcustom <- function(n) {
    stop("failed instantly")
  }

  assign("rcustom", value=rcustom, envir=.GlobalEnv)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="rcustom")

  expect_error(sim_from_dag(dag, n=10),
               paste0("An error occured when processing root node 'B'. ",
                      "The message was: Error in (function (n) : ",
                      "failed instantly"),
               fixed=TRUE)
})

test_that("helpful error message child nodes", {

  node_custom <- function(data, parents) {
    stop("failed instantly")
  }

  assign("node_custom", value=node_custom, envir=.GlobalEnv)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="custom", parents="A")

  expect_error(sim_from_dag(dag, n=10),
               paste0("An error occured when processing node 'B'. ",
                      "The message was: Error in ",
                      "(function (data, parents) : failed instantly"),
               fixed=TRUE)
})
