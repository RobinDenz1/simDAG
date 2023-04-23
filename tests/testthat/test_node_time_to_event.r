set.seed(42)

dt <- data.table::data.table(.id=seq(1, 200),
                             "age"=rnorm(n=200, mean=30, sd=7.5),
                             "sex"=rbinom(n=200, size=1, prob=0.7))
dt <- dt %>%
  dplyr::mutate(
    smoking = ifelse(dt$sex == 0,
                     rbinom(n=nrow(dt[dt$sex == 0,]), size=1, prob=0.26),
                     rbinom(n=nrow(dt[dt$sex == 1,]), size=1, prob=0.20)),
    sickness_event = FALSE,
    sickness_time = NA_integer_,
    sickness_past_event_times = NA_integer_)

prob_sick <- function(data, rr_smoke0, rr_smoke1) {
  # smoking-dependent risk
  risk <- fifelse(data$smoking == 1, rr_smoke0, rr_smoke1)

  # sex- and age-dependet baseline risk
  base_p <- rep(0.1, nrow(data))

  base_p[data$sex == 0  & data$age < 18] <- 0
  base_p[data$sex == 0  & data$age >= 18 & data$age < 44] <- 0.004
  base_p[data$sex == 0  & data$age >= 45 & data$age < 54] <- 0.034
  base_p[data$sex == 0  & data$age >= 55 & data$age < 64] <- 0.077
  base_p[data$sex == 0  & data$age >= 65 & data$age < 74] <- 0.130
  base_p[data$sex == 0  & data$age >= 75] <- 0.241

  base_p[data$sex == 1  & data$age < 18] <- 0
  base_p[data$sex == 1  & data$age >= 18 & data$age < 44] <- 0.002
  base_p[data$sex == 1  & data$age >= 45 & data$age < 54] <- 0.009
  base_p[data$sex == 1  & data$age >= 55 & data$age < 64] <- 0.034
  base_p[data$sex == 1  & data$age >= 65 & data$age < 74] <- 0.071
  base_p[data$sex == 1  & data$age >= 75] <- 0.160

  p <- base_p * risk

  return(p)
}

test_that("correct nrow, ncol", {
  out <- node_time_to_event(dt,
                            parents=c("age", "sex", "smoking"),
                            sim_time=100,
                            name="sickness",
                            prob_fun=prob_sick,
                            prob_fun_args=list(rr_smoke0=1, rr_smoke1=5),
                            event_duration=1,
                            immunity_duration=100,
                            save_past_events=FALSE,
                            envir=NULL)

  expect_true(data.table::is.data.table(out))
  expect_true(nrow(out) == 200)
  expect_true(ncol(out) == 2)
  expect_equal(colnames(out), c("sickness_event", "sickness_time"))
})

test_that("correct sim_time", {
  out <- node_time_to_event(dt,
                            parents=c("age", "sex", "smoking"),
                            sim_time=50,
                            name="sickness",
                            prob_fun=prob_sick,
                            prob_fun_args=list(rr_smoke0=1, rr_smoke1=5),
                            event_duration=1,
                            immunity_duration=100,
                            save_past_events=FALSE,
                            envir=NULL)

  expect_true(unique(out[sickness_event == TRUE]$sickness_time) == 50)
})

test_that("save_past_events working", {

  # setup that would usually happen in sim_discrete_time
  envir <- environment()
  past_events_list <- list(
    sickness_past_event_times=vector(mode="list", length=100))

  # calling the function once
  out1 <- node_time_to_event(dt,
                             parents=c("age", "sex", "smoking"),
                             sim_time=100,
                             name="sickness",
                             prob_fun=prob_sick,
                             prob_fun_args=list(rr_smoke0=10, rr_smoke1=15),
                             event_duration=1,
                             immunity_duration=100,
                             save_past_events=TRUE,
                             envir=envir)
  expect_true(!is.null(past_events_list$sickness[[100]]))
  expect_true(is.null(past_events_list$sickness[[1]]))
})
