
dag <- empty_dag() +
  node("A", "rnorm", mean=10, sd=120) +
  node("B", "rbernoulli", p=0.3)

dag_td <- empty_dag() +
  node_td("A", "time_to_event", prob_fun=0.01)

dag_des <- empty_dag() +
  node_td("Y", type="next_time", prob_fun=0.01, event_duration=Inf)

test_that("without td, basecase", {
  out <- sim_n_datasets(dag=dag, n_repeats=10, n_cores=1, n_sim=10)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "data.table"))
})

test_that("without td, data_format", {

  dat_transform <- function(data) {
    return(10)
  }

  assign("dat_transform", value=dat_transform, envir=.GlobalEnv)

  out <- sim_n_datasets(dag=dag, n_repeats=10, n_cores=1, n_sim=10,
                        data_format="dat_transform")
  expect_true(length(out) == 10)
  expect_equal(out[[1]], 10)
})

test_that("with td, basecase", {
  out <- sim_n_datasets(dag=dag_td, n_repeats=10, n_cores=1, n_sim=10,
                        max_t=100)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "simDT"))
})

test_that("with td, data_format", {
  out <- sim_n_datasets(dag=dag_td, n_repeats=10, n_cores=1, n_sim=10,
                        max_t=100, data_format="start_stop")
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "data.table"))
})

test_that("without td, parallel", {
  out <- sim_n_datasets(dag=dag, n_repeats=10, n_cores=2, n_sim=10)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "data.table"))
})

test_that("with td, parallel", {
  out <- sim_n_datasets(dag=dag_td, n_repeats=10, n_cores=2, n_sim=10,
                        max_t=100, progressbar=FALSE)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "simDT"))
})

test_that("with td, parallel", {
  expect_output(sim_n_datasets(dag=dag_td, n_repeats=10, n_cores=2, n_sim=10,
                               max_t=100))
})

test_that("with DES, basecase", {
  out <- sim_n_datasets(dag=dag_des, n_repeats=10, n_cores=1, n_sim=10,
                        max_t=100)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "data.table"))
})

test_that("with DES, parallel", {
  out <- sim_n_datasets(dag=dag_des, n_repeats=10, n_cores=2, n_sim=10,
                        max_t=100)
  expect_true(length(out) == 10)
  expect_true(inherits(out[[1]], "data.table"))
})

test_that("with DES, data_format warning", {
  expect_warning(sim_n_datasets(dag=dag_des, n_repeats=10, n_cores=1, n_sim=10,
                                max_t=100, data_format="long"),
                 paste0("Only the 'start_stop' format is supported ",
                        "with discrete-event simulations. No data ",
                        "transformation was carried out."))
})

test_that("custom function for prob_fun, parallel", {

  fun2 <- function() {
    return(1)
  }

  prob_fun <- function(data) {
    test <- fun2()
    return(0.1)
  }

  assign("prob_fun", value=prob_fun, envir=.GlobalEnv)
  assign("fun2", value=fun2, envir=.GlobalEnv)

  dag <- empty_dag() +
    node("A", "rnorm", mean=10, sd=120) +
    node("B", "rbernoulli", p=0.3) +
    node_td("C", "time_to_event", prob_fun=prob_fun)

  expect_no_error(out <- sim_n_datasets(dag=dag, n_sim=100, n_repeats=2,
                                        max_t=10, n_cores=2))
})

test_that("custom function for data_format, parallel", {

  data_fun <- function(data) {
    return(data)
  }

  assign("data_fun", value=data_fun, envir=.GlobalEnv)

  dag <- empty_dag() +
    node("A", "rnorm", mean=10, sd=120) +
    node("B", "rbernoulli", p=0.3) +
    node_td("C", "time_to_event", prob_fun=0.01)

  expect_no_error(out <- sim_n_datasets(dag=dag, n_sim=100, n_repeats=2,
                                        max_t=10, n_cores=2,
                                        data_format=data_fun))
})
