
root_nodes <- list(list(type_str="rnorm",
                        type_fun=rnorm,
                        params=list(mean=17, sd=4),
                        name="age",
                        time_varying=FALSE),
                   list(type_str="rbernoulli",
                        type_fun=rbernoulli,
                        params=list(p=0.7),
                        name="sex",
                        time_varying=FALSE))
child_nodes <- list(list(parents=c("sex", "age"),
                         type_str="gaussian",
                         type_fun=node_gaussian,
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
                           type_str="gaussian",
                           type_fun=node_gaussian,
                           name="bmi",
                           betas=c(2.1, 1.4, 0.1),
                           intercept=14,
                           error=2,
                           time_varying=FALSE),
                      list(parents=c("sex", "age"),
                           type_str="gaussian",
                           type_fun=node_gaussian,
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

test_that("accepts nodes directly passed in type", {

  custom_fun <- function(data, parents) {
    rbernoulli(n=nrow(data))
  }

  dag <- empty_dag() +
    node("A", type=rbernoulli, p=0.4, output="numeric") +
    node("B", type=node_gaussian, formula=~ -1 + A*3, error=2) +
    node("C", type=custom_fun, parents=c("A", "B"))
  dat <- sim_from_dag(dag, n_sim=10)

  expect_true(nrow(dat)==10)
  expect_true(ncol(dat)==3)
  expect_true(is.numeric(dat$B))
  expect_true(is.logical(dat$C))
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
                      "The message was:\nError in (function (n) : ",
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
                      "The message was:\nError in ",
                      "(function (data, parents) : failed instantly"),
               fixed=TRUE)
})

test_that("helpful error message formula error", {

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("B", type="gaussian", formula= ~ -1 + A*2 + C*3)

  expect_error(sim_from_dag(dag, n_sim=100),
               paste0("An error occured when interpreting the formula of ",
               "node 'B'. The message was:\nError: Error in `[.data.table`",
               "(mod_mat, , args$parents, with = FALSE): column(s) not ",
               "found: [A, C]\nThis error may occur when one of the terms ",
               "in a supplied formula does not match any variables in ",
               "the generated data.\n Please check whether all terms in ",
               "your supplied formula occur in the data generated up ",
               "to this point.\n The variables currently available in",
               " data are:\n(Intercept), ATRUE"), fixed=TRUE)

})
