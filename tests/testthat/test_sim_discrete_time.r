
set.seed(42)

dt <- data.table::data.table(.id=seq(1, 200),
                             "age" = rnorm(n = 200, mean = 30, sd = 7.5),
                             "sex" = rbinom(n = 200, size = 1, prob = 0.7))

prob_sick1 <- function(data, rr_sex0, rr_sex1) {
  # sex-dependent risk
  risk <- fifelse(data$sex == 0, rr_sex0, rr_sex1)

  # age-dependent baseline risk
  base_p <- rep(0.1, nrow(data))
  base_p[data$age >= 0 & data$age < 12] <- 0.0001
  base_p[data$age >= 12 & data$age < 18] <- 0.0005
  base_p[data$age >= 18 & data$age < 25] <- 0.0010
  base_p[data$age >= 25 & data$age < 35] <- 0.0020
  base_p[data$age >= 35 & data$age < 45] <- 0.0030
  base_p[data$age >= 45 & data$age < 55] <- 0.0060
  base_p[data$age >= 55 & data$age < 65] <- 0.0160
  base_p[data$age >= 65 & data$age < 75] <- 0.0350
  base_p[data$age >= 75 & data$age < 85] <- 0.0950
  base_p[data$age >= 85] <- 0.2900

  p <- base_p * risk

  return(p)
}

prob_sick2 <- function(data, rr_sex0, rr_sex1) {
  # sex-dependent risk
  risk <- fifelse(data$sex == 0, rr_sex0, rr_sex1)

  # age-dependent baseline risk
  base_p <- rep(0.1, nrow(data))
  base_p[data$age >= 0 & data$age < 12] <- 0.001
  base_p[data$age >= 12 & data$age < 18] <- 0.005
  base_p[data$age >= 18 & data$age < 25] <- 0.010
  base_p[data$age >= 25 & data$age < 35] <- 0.020
  base_p[data$age >= 35 & data$age < 45] <- 0.030
  base_p[data$age >= 45 & data$age < 55] <- 0.060
  base_p[data$age >= 55 & data$age < 65] <- 0.150
  base_p[data$age >= 65 & data$age < 75] <- 0.350
  base_p[data$age >= 75 & data$age < 85] <- 0.950
  base_p[data$age >= 85] <- 0.990

  p <- base_p * risk

  return(p)
}

dag <- empty_dag() +
  node_td("sickness1", parents=c("age", "sex"), type="time_to_event",
          prob_fun=prob_sick1, rr_sex0=3, rr_sex1=1,
          event_duration=14, immunity_duration=30, save_past_events=FALSE)

test_that("correct nrow, ncol", {
  sim_dat <- sim_discrete_time(t0_data=dt,
                               max_t=365,
                               dag=dag)$data

  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat) == 200)
  expect_true(ncol(sim_dat) == 5)
})

test_that("tx_nodes_order working", {

  dag <- empty_dag() +
    node_td("sickness1", parents=c("age", "sex"), type="time_to_event",
            prob_fun=prob_sick1, rr_sex0=3, rr_sex1=1,
            event_duration=14, immunity_duration=30, save_past_events=FALSE) +
    node_td("sickness2", parents=c("age", "sex"), type="time_to_event",
            prob_fun=prob_sick2, rr_sex0=1, rr_sex1=2,
            event_duration=14, immunity_duration=100, save_past_events=FALSE)

  sim_dat <- suppressWarnings({
    sim_discrete_time(t0_data=dt,
                      max_t=5,
                      dag=dag,
                      tx_nodes_order=c(2, 1),
                      verbose=TRUE)$data
    })

  expect_true(data.table::is.data.table(sim_dat))
  expect_true(nrow(sim_dat) == 200)
  expect_true(ncol(sim_dat) == 7)
  expect_output(
    suppressWarnings({
      sim_discrete_time(t0_data = dt,
                        max_t = 5,
                        dag = dag,
                        tx_nodes_order = c(2, 1),
                        verbose = TRUE)
      }),
    "t = 1 node = sickness2\\nt = 1 node = sickness1")
})

test_that("save_states working", {
  sim_dat_all <- sim_discrete_time(t0_data = dt,
                                   max_t = 365,
                                   dag = dag,
                                   save_states = "all")$data

  expect_true(ncol(sim_dat_all) == 5)

  sim_dat_t <- sim_discrete_time(t0_data = dt,
                                 max_t = 365,
                                 dag = dag,
                                 save_states = "at_t",
                                 save_states_at = 100)$data

  expect_true(ncol(sim_dat_all) == 5)
})

test_that("verbose working", {
  expect_output(sim_discrete_time(t0_data = dt,
                                  max_t = 365,
                                  dag = dag,
                                  verbose = TRUE))
  expect_output(sim_discrete_time(t0_data = dt,
                                  max_t = 365,
                                  dag = dag,
                                  verbose = TRUE),
                "t = 365 node = sickness1")
})

test_that("error when not a DAG object", {
  expect_error(sim_discrete_time(dag="1", n_sim=100, max_t=12))
})

test_that("using t0_transform_fun", {

  trans_fun <- function(data, a) {
    data$age <- data$age * 2
    return(data)
  }

  sim <- sim_discrete_time(t0_data=dt,
                           max_t=365,
                           dag=dag,
                           verbose=FALSE,
                           t0_transform_fun=trans_fun,
                           t0_transform_args=list(a=10))

  expect_equal(sim$data$age / 2, dt$age)
})

test_that("using tx_transform_fun", {

  trans_fun <- function(data, a) {
    data$age <- data$age + a
    return(data)
  }

  sim <- sim_discrete_time(t0_data=dt,
                           max_t=100,
                           dag=dag,
                           verbose=FALSE,
                           tx_transform_fun=trans_fun,
                           tx_transform_args=list(a=1))

  expect_equal(sim$data$age - 100, dt$age)
})

test_that("helpful error message node processing working", {

  # function that purposefully throws an error at t = 100
  prob_car_crash <- function(data, sim_time, base_p) {

    if (sim_time != 100) {
      base_p + sim_time * 0.0001
    } else {
      stop("Error at t = 100")
    }

    return(base_p)
  }

  dag <- empty_dag() +
    node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
            event_duration=1, base_p=0.0001)

  expect_error(sim_discrete_time(dag=dag, n_sim=10, max_t=120),
               paste0("An error occured when processing node 'car_crash'",
                      " at time t = 100. The message was: Error in (",
                      "function (data, sim_time, base_p) : Error at",
                      " t = 100"), fixed=TRUE)
})

test_that("helpful error message adding node to data working", {

  dag <- empty_dag() +
    node_td("custom_nonsense", type="nonsense")

  node_nonsense <- function(data, sim_time) {
    if (sim_time < 100) {
      return(10)
    } else {
      return(list(20, 12))
    }
  }

  assign("node_nonsense", value=node_nonsense, envir=.GlobalEnv)

  expect_error(sim_discrete_time(dag=dag, n_sim=10, max_t=120),
               paste0("An error occured when trying to add the output of ",
                      "node 'custom_nonsense' at time t = 100 to the ",
                      "current data. The message was: Error in ",
                      "`[<-.data.table`(`*tmp*`, , name, value = list(20, ",
                      "12)): Supplied 2 items to be assigned to 10 items ",
                      "of column 'custom_nonsense'. If you wish to ",
                      "'recycle' the RHS please use rep() to make this ",
                      "intent clear to readers of your code."),
               fixed=TRUE)
})

test_that("helpful error message when processing tx_transform_fun", {

  # function that purposefully throws an error at t = 100
  transform_fun <- function(data) {
    stop("failed instantly")
  }

  dag <- empty_dag() +
    node_td("car_crash", type="time_to_event", prob_fun=0.001,
            event_duration=1)

  expect_error(sim_discrete_time(dag=dag, n_sim=10, max_t=120,
                                 tx_transform_fun=transform_fun),
               paste0("An error occured when calling the tx_transform() ",
                      "function at t = 1. The message was: Error in ",
                      "(function (data) : failed instantly"), fixed=TRUE)
})

test_that("using a custom node", {

  dag <- empty_dag() +
    node_td("custom_nonsense", type="sim_time_multiplier")

  node_sim_time_multiplier <- function(data, sim_time) {
    return(sim_time * 2)
  }

  # function needs to be global
  assign("node_sim_time_multiplier", value=node_sim_time_multiplier,
         envir=.GlobalEnv)

  sim <- sim_discrete_time(dag=dag, n_sim=100, max_t=20)

  expect_equal(sim$data$custom_nonsense, rep(40, 100))
})
