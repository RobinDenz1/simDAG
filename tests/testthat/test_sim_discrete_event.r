
prob_Y <- function(data, base_p, rr_A, rr_X1, rr_X2, rr_X3) {
  base_p * rr_X1^(data$X1) * rr_X2^(data$X2) * rr_X3^(data$X3) *
    rr_A^(data$A)
}

prob_X <- function(data, base_p) {
  return(base_p)
}

test_that("general test case", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf)

  sim[, max_t := max(stop, na.rm=TRUE), by=.id]
  sim[, sum_event := X1 + X2 + X3 + Y]
  sim[, index := seq_len(.N)-1, by=.id]

  d_final <- subset(sim, start==max_t)

  expect_true(nrow(sim)==5000)
  expect_true(all(d_final$sum_event==4))
  expect_true(all(sim$sum_event==sim$index))

  # test with target_event="Y"
  set.seed(1234)
  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y")

  expect_true(nrow(sim)==5000)
  expect_true(all(d_final$sum_event==4))
  expect_true(all(sim$sum_event==sim$index))
})

test_that("max_t working", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  # some fewer rows than 5000, cause not all individuals "finished"
  sim <- sim_discrete_event(dag, n_sim=1000, max_t=700)
  expect_equal(nrow(sim), 4739)

  # censor_at_max_t also works
  sim <- sim_discrete_event(dag, n_sim=1000, max_t=700, censor_at_max_t=TRUE)
  sim <- sim[, .(actual_max_t = max(stop)), by=.id]
  expect_true(all(sim$actual_max_t <= 700))
})

test_that("using a single number in prob_fun works", {

  set.seed(12344)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=0.4) +
    node_td("X2", type="next_time", prob_fun=1.3) +
    node_td("X3", type="next_time", prob_fun=0.001) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf)
  expect_equal(nrow(sim), 5000)
  expect_equal(round(mean(sim$start), 3), 287.271)
  expect_equal(round(mean(sim[is.finite(stop)]$stop), 3), 359.089)
})

test_that("unrelated variables are equal to rtexp()", {

  set.seed(653546)

  # just one
  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.001)

  sim <- sim_discrete_event(dag, n_sim=100000, max_t=Inf)
  d_max_t <- sim[, .(max_t = max(start)), by=.id]

  q_DES <- quantile(d_max_t$max_t, probs=c(0.2, 0.5, 0.8))
  q_rexp <- quantile(stats::rexp(n=1000000, rate=0.001),
                     probs=c(0.2, 0.5, 0.8))

  expect_equal(round(as.vector(q_DES)), c(225, 698, 1612))
  expect_equal(round(as.vector(q_rexp)), c(223, 692, 1608))

  # multiple events
  dag <- empty_dag() +
    node_td(c("Y1", "Y2"), type="next_time", prob_fun=0.001)

  sim <- sim_discrete_event(dag, n_sim=100000, max_t=Inf)

  d_max_t <- sim[, .(
    Y1_first = start[which(Y1)[1]],
    Y2_first = start[which(Y2)[1]]
  ), by = .id]

  q_Y1 <- quantile(d_max_t$Y1_first, probs=c(0.2, 0.5, 0.8))
  q_Y2 <- quantile(d_max_t$Y2_first, probs=c(0.2, 0.5, 0.8))

  expect_equal(round(as.vector(q_Y1)), c(222, 690, 1604))
  expect_equal(round(as.vector(q_Y2)), c(223, 696, 1606))
})

test_that("using .event_count", {

  prob_X2 <- function(data) {
    0.001 * 5^(data$.event_count)
  }

  set.seed(123446)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X", type="next_time", prob_fun=0.001) +
    node_td("Y", type="next_time", prob_fun=prob_X2, event_duration=100)

  expect_no_error(sim_discrete_event(dag, n_sim=100, max_t=10000,
                                     remove_if=X==TRUE))
})

test_that("custom distr_fun working", {

  rtexp_plus_1 <- function(n, rate, l) {
    rtexp(n=n, rate=rate, l=l) + 1
  }

  set.seed(123446)
  dag <- empty_dag() +
    node_td("X", type="next_time", prob_fun=0.001, distr_fun=rtexp_plus_1)

  sim1 <- sim_discrete_event(dag, n_sim=10, max_t=Inf)
  sim1[, is_first := seq_len(.N)==1, by=.id]
  sim1 <- subset(sim1, is_first==TRUE)

  set.seed(123446)
  dag <- empty_dag() +
    node_td("X", type="next_time", prob_fun=0.001, distr_fun=rtexp)

  sim2 <- sim_discrete_event(dag, n_sim=10, max_t=Inf)
  sim2[, is_first := seq_len(.N)==1, by=.id]
  sim2 <- subset(sim2, is_first==TRUE)

  expect_true(all(sim2$stop==(sim1$stop-1)))
})

test_that("event_duration working", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.001,
            event_duration=1000) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001,
            event_duration=700) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            event_duration=Inf) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                            remove_if=Y==TRUE, censor_at_max_t=TRUE)

  # all 1000 (or Inf if stop > max_t) in X1
  sim[, grp := rleid(.id, X1)]
  res_X1 <- sim[X1 == TRUE,
                 .(event_start = data.table::first(start),
                   event_stop  = data.table::last(stop),
                   duration    = data.table::last(stop) -
                     data.table::first(start)),
                 by = .(.id, grp)
  ][, grp := NULL]

  expect_true(all(round(res_X1$duration, 10)==1000 |
                    is.infinite(res_X1$duration)))

  # all 700 (or Inf if stop > max_t) in X2
  sim[, grp := data.table::rleid(.id, X2)]
  res_X2 <- sim[X2 == TRUE,
                .(event_start = data.table::first(start),
                  event_stop  = data.table::last(stop),
                  duration    = data.table::last(stop) -
                    data.table::first(start)),
                by = .(.id, grp)
  ][, grp := NULL]

  expect_true(all(round(res_X2$duration, 10)==700 |
                    is.infinite(res_X2$duration)))

  # only one event for X3
  sim[, grp := rleid(.id, X3)]
  sim <- subset(sim, X3==TRUE)
  res_X3 <- sim[, .(n = uniqueN(grp)), by=.id]

  expect_true(all(res_X3$n==1))
})

test_that("immunity_duration working", {

  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01,
            event_duration=10, immunity_duration=750) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.01,
            event_duration=45, immunity_duration=1000) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            event_duration=200, immunity_duration=Inf) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                            remove_if=Y==TRUE, censor_at_max_t=TRUE)

  # all event-less times are at least 740 in X1
  sim[, grp := rleid(.id, X1)]
  res_X1 <- sim[X1 == FALSE ,
                .(event_start = data.table::first(start),
                  event_stop  = data.table::last(stop),
                  duration    = data.table::last(stop) -
                    data.table::first(start)),
                by = .(.id, grp)
  ][, grp := NULL]
  res_X1[, is_first_row := seq_len(.N)==1, by=.id]
  res_X1 <- subset(res_X1, is_first_row==FALSE)

  expect_true(min(res_X1) < 740)

  # all event-less times (or Inf if stop > max_t) in X2
  sim[, grp := rleid(.id, X2)]
  res_X2 <- sim[X2 == FALSE ,
                .(event_start = data.table::first(start),
                  event_stop  = data.table::last(stop),
                  duration    = data.table::last(stop) -
                    data.table::first(start)),
                by = .(.id, grp)
  ][, grp := NULL]
  res_X2[, is_first_row := seq_len(.N)==1, by=.id]
  res_X2 <- subset(res_X2, is_first_row==FALSE)

  expect_true(min(res_X2) < 955)

  # only one event for X3
  sim[, grp := rleid(.id, X3)]
  sim <- subset(sim, X3==TRUE)
  res_X3 <- sim[, .(n = uniqueN(grp)), by=.id]

  expect_true(all(res_X3$n==1))
})

test_that("supplying t0_data", {

  dagt0 <- empty_dag() +
    node("A", type="rbernoulli")

  t0_data <- sim_from_dag(dagt0, n_sim=1000)

  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.01)

  sim <- sim_discrete_event(dag, t0_data=copy(t0_data), max_t=Inf)
  sim <- subset(sim, is.finite(stop))
  expect_equal(sim$A, t0_data$A)

  # warning if n_sim is specified anyways
  expect_warning(sim_discrete_event(dag, n_sim=10, t0_data=t0_data))
})

test_that("using t0_transform_fun", {

  dag <- empty_dag() +
    node("A", type="rconstant", constant=10) +
    node_td("Y", type="next_time", prob_fun=0.01)

  test_fun <- function(data, value) {
    data$A <- data$A + value
    return(data)
  }

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                            t0_transform_fun=test_fun,
                            t0_transform_args=list(value=10))
  expect_true(all(sim$A==20))

  # error when arg is not defined in function
  expect_error(sim_discrete_event(dag, n_sim=100, t0_transform_fun=test_fun,
                                  t0_transform_args=list(value=10, value2=1)))
})

test_that("redraw_at_t working", {

  set.seed(12349)

  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.0000001)

  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                            redraw_at_t=c(45, 400, 1220))
  sim <- subset(sim, is.finite(stop))

  # NOTE: earlier events would be possible, so this isn't always TRUE
  expect_equal(sim$start, rep(c(0, 45, 400, 1220), 1000))
})

test_that("interrelated time-dependent events", {

  # probability function for L(t)
  prob_L <- function(data, base_p, rr_A, rr_M, rr_X) {
    p <- base_p * rr_M^(data$M_event) * rr_A^(data$A) * rr_X^(data$X)
    return(p)
  }

  # probability function for M(t)
  prob_M <- function(data, base_p, rr_A, rr_L, rr_X) {
    p <- base_p * rr_L^(data$L_event) * rr_A^(data$A) * rr_X^(data$X)
    return(p)
  }

  # probability function for Y(t)
  prob_Y <- function(data, base_p, rr_A, rr_M, rr_L, rr_X) {
    p <- base_p * rr_M^(data$M_event) * rr_L^(data$L_event) * rr_A^(data$A) *
      rr_X^(data$X)
    return(p)
  }

  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node("A", type="rbernoulli", p=0.5) +
    node_td("L_event", type="next_time", prob_fun=prob_L,
            event_duration=Inf,
            base_p=0.01, rr_A=0.6, rr_M=0.6, rr_X=0.8,
            parents=c("M_event", "A", "X")) +
    node_td("M_event", type="next_time", prob_fun=prob_M,
            event_duration=Inf,
            base_p=0.01, rr_A=0.2, rr_L=3, rr_X=0.8,
            parents=c("L_event", "A", "X")) +
    node_td("Y_event", type="next_time", prob_fun=prob_Y,
            parents=c("M_event", "L_event", "A", "X"),
            event_duration=Inf, base_p=0.0001,
            rr_A=0.9, rr_M=5, rr_L=2.5, rr_X=0.8)

  set.seed(23452143)
  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf)
  expect_equal(round(mean(sim$start), 3), 349.783)
})

test_that("target_event and censor_at_max_t", {

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  # without keep_only_first, nothing changes
  set.seed(1234)
  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y")

  set.seed(1234)
  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y",
                             censor_at_max_t=TRUE)
  expect_equal(sim1, sim2)

  # with keep_only_first, there is no difference at all either
  set.seed(1234)
  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y",
                             keep_only_first=TRUE)

  set.seed(1234)
  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y",
                             censor_at_max_t=TRUE, keep_only_first=TRUE)
  expect_equal(sim1, sim2)
})

test_that("target_event functionality working", {

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  set.seed(123454)
  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y")

  # event is never in the last row
  sim1[, count := seq_len(.N), by=.id]
  last_row <- sim1[count==5]

  expect_true(!any(last_row$Y))

  # with keep_only_first, event is always in the last row
  set.seed(123454)
  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y",
                             keep_only_first=TRUE)

  sim1[, count := seq_len(.N), by=.id]
  last_row <- sim1[count==5]

  expect_true(all(last_row$Y))

  # removing not at risk leads to the same results as keep_only_first
  # since immunity_duration = Inf
  set.seed(123454)
  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, target_event="Y",
                             remove_not_at_risk=TRUE)

  sim1[, count := NULL]
  expect_equal(sim1, sim2)

  # no longer the same with recurring event
  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7,
            event_duration=100, immunity_duration=500)

  set.seed(123454)
  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=2000, target_event="Y",
                             keep_only_first=TRUE)

  set.seed(123454)
  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=2000, target_event="Y",
                             remove_not_at_risk=TRUE)

  expect_equal(nrow(sim1), 3394)
  expect_equal(nrow(sim2), 6182)
})

test_that("break_if working", {

  dag <- empty_dag() +
    node_td(c("Y1", "Y2", "Y3"), type="next_time", prob_fun=0.001,
            event_duration=50)

  set.seed(1324)
  expect_no_error(sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                                     break_if=any(data$.time > 4000)))
})

test_that("same output for allow_ties when no ties are present", {

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  set.seed(1234)
  sim_no_ties <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                                    allow_ties=FALSE)

  set.seed(1234)
  sim_with_ties <- sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                                      allow_ties=TRUE)

  expect_equal(sim_no_ties, sim_with_ties)
})

test_that("works with actual ties", {

  round_rtexp <- function(n, rate, l) {
    round(rtexp(n=n, rate=rate, l=l))
  }

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01,
            distr_fun=round_rtexp) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001,
            distr_fun=round_rtexp) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            distr_fun=round_rtexp) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7,
            distr_fun=round_rtexp)

  set.seed(1234)

  # fails if ties are present but allow_ties=FALSE
  expect_error(sim_discrete_event(dag, n_sim=1000, max_t=Inf,
                                  allow_ties=FALSE),
               paste0("Multiple changes at the same point in time ",
               "occurred, but allow_ties=FALSE was used. ",
               "Set allow_ties=TRUE and re-run this function."))

  # less than 5 * 1000 rows, because some individuals had ties
  sim <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, allow_ties=TRUE)
  expect_equal(nrow(sim), 4989)
})

test_that("node_next_time returns NULL", {
  expect_null(node_next_time())
})

test_that("using event_count=TRUE is same as .event_count in same node", {

  prob_X2 <- function(data, kind) {
    if (kind==1) {
      0.001 * 5^(data$Y_event_count)
    } else {
      0.001 * 5^(data$.event_count)
    }
  }

  # using .event_count
  set.seed(123446)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X", type="next_time", prob_fun=0.001) +
    node_td("Y", type="next_time", prob_fun=prob_X2, event_duration=100,
            event_count=TRUE, kind=2)
  sim1 <- sim_discrete_event(dag, n_sim=100, max_t=1000, remove_if=X==TRUE)

  # using event_count=TRUE
  set.seed(123446)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X", type="next_time", prob_fun=0.001) +
    node_td("Y", type="next_time", prob_fun=prob_X2, event_duration=100,
            event_count=TRUE, kind=1)
  sim2 <- sim_discrete_event(dag, n_sim=100, max_t=1000, remove_if=X==TRUE)

  expect_equal(sim1, sim2)

  # the latter allows dependency of event counts on other events
  set.seed(123446)

  prob_X3 <- function(data) {
    0.001 * 1.2^(data$Y_event_count)
  }

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X", type="next_time", prob_fun=prob_X3) +
    node_td("Y", type="next_time", prob_fun=prob_X2, event_duration=100,
            event_count=TRUE, kind=1)
  expect_no_error(sim_discrete_event(dag, n_sim=100, max_t=1000,
                                     remove_if=X==TRUE))

  # using the include_event_counts argument works
  set.seed(1234)
  sim1 <- sim_discrete_event(dag, n_sim=100, max_t=1000,
                             remove_if=X==TRUE)
  set.seed(1234)
  sim2 <- sim_discrete_event(dag, n_sim=100, max_t=1000,
                             remove_if=X==TRUE, include_event_counts=FALSE)

  sim1[, Y_event_count := NULL]
  expect_equal(sim1, sim2)
})

test_that("equal formula and prob_fun give equivalent results", {

  # NOTE: rounding is used here, because otherwise the results are not
  #       *exactly* the same due to floating point errors
  round_rtexp <- function(n, rate, l) {
    ceiling(rtexp(n=n, rate=rate, l=l))
  }

  # using prob_fun
  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01,
            distr_fun=round_rtexp) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001,
            distr_fun=round_rtexp) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            distr_fun=round_rtexp) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7,
            distr_fun=round_rtexp)

  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, allow_ties=TRUE)

  # using formula
  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01,
            distr_fun=round_rtexp) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001,
            distr_fun=round_rtexp) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            distr_fun=round_rtexp) +
    node_td("Y", type="next_time",
            formula= ~ log(0.001) + A*log(0.8) + X1*log(1.5) + X2*log(2) +
              X3*log(0.7), link="log", distr_fun=round_rtexp)

  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf, allow_ties=TRUE)

  expect_equal(sim1, sim2)
})

test_that("different links with formula work", {

  # log link
  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time",
            formula= ~ log(0.001) + A*log(0.8) + X1*log(1.5) + X2*log(2) +
              X3*log(0.7), link="log")

  sim1 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf)

  # logit link
  set.seed(1234)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time",
            formula= ~ log(0.001) + A*log(0.8) + X1*log(1.5) + X2*log(2) +
              X3*log(0.7), link="logit")

  sim2 <- sim_discrete_event(dag, n_sim=1000, max_t=Inf)

  expect_true(!all(sim1$start==sim2$start))
})

test_that("formula works with remove_if", {

  set.seed(134)

  dag <- empty_dag() +
    node_td("treatment", type="next_time", prob_fun=0.01,
            event_duration=100) +
    node_td("death", type="next_time",
            formula= ~ log(0.001) + log(0.8)*treatment, link="log",
            event_duration=Inf)

  sim <- sim_discrete_event(dag, n_sim=10, remove_if=death==TRUE,
                            target_event="death")
  expect_equal(nrow(sim), 108)
})

test_that("warning if max_iter reached", {

  set.seed(356345)

  dag <- empty_dag() +
    node_td("A", type="next_time", prob_fun=0.01, event_duration=10)

  expect_warning(sim_discrete_event(dag, n_sim=100, max_iter=10))
})

test_that("error with no dag", {
  expect_error(sim_discrete_event(dag=1, n_sim=100, max_t=Inf))
})

test_that("helpful error if prob_fun fails", {

  test_fun <- function(data) {
    stop("This is an error")
  }

  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=test_fun, event_duration=Inf)

  expect_error(sim_discrete_event(dag, n_sim=10))
})

test_that("helpful error if distr_fun fails", {

  test_fun <- function(n, rate, l) {
    stop("This is an error")
  }

  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.01, event_duration=Inf,
            distr_fun=test_fun)

  expect_error(sim_discrete_event(dag, n_sim=10))
})

test_that("warning with networks in DAG and remove_if", {

  set.seed(1234)

  g1 <- igraph::sample_gnm(n=50, m=35)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    network("g1", net=g1) +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01,
            event_duration=10, immunity_duration=750) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.01,
            event_duration=45, immunity_duration=1000) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02,
            event_duration=200, immunity_duration=Inf) +
    node_td("Y", type="next_time", prob_fun=prob_Y, base_p=0.001,
            rr_A=0.8, rr_X1=1.5, rr_X2=2, rr_X3=0.7)

  expect_warning(sim_discrete_event(dag, n_sim=50, max_t=Inf,
                                    remove_if=Y==TRUE, censor_at_max_t=TRUE))
})

test_that("error with time-dependent networks", {

  gen_network <- function(n_sim) {
    return(NULL)
  }

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node_td("Y", type="next_time", prob_fun=0.001) +
    network_td("some_network", net=gen_network)

  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf),
               paste0("Time-dependent networks are currently not ",
                      "supported in the sim_discrete_event() function."),
               fixed=TRUE)
})

test_that("error with net() terms in formula", {

  set.seed(1234)

  g1 <- igraph::sample_gnm(n=100, m=35)

  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    network("g1", net=g1) +
    node_td("X1", type="next_time", prob_fun=prob_X, base_p=0.01) +
    node_td("X2", type="next_time", prob_fun=prob_X, base_p=0.001) +
    node_td("X3", type="next_time", prob_fun=prob_X, base_p=0.02) +
    node_td("Y", type="next_time",
            formula = ~ log(0.001) + A*log(0.8) + X1*log(1.5) + X2*log(2) +
              X3*log(0.7) + net(mean(X1))*0.1, link="logit")

  expect_error(sim_discrete_event(dag, n_sim=100, max_t=Inf, allow_ties=FALSE))
})
