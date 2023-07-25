
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
