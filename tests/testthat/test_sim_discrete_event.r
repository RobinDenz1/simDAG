
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
  expect_equal(round(mean(sim$stop, na.rm=TRUE), 3), 359.089)
})

test_that("unrelated variables are equal to rtexp()", {

  set.seed(653546)

  # just one
  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.001)

  sim <- sim_discrete_event(dag, n_sim=1000000, max_t=Inf)
  d_max_t <- sim[, .(max_t = max(start)), by=.id]

  q_DES <- quantile(d_max_t$max_t, probs=c(0.2, 0.5, 0.8))
  q_rexp <- quantile(stats::rexp(n=1000000, rate=0.001),
                     probs=c(0.2, 0.5, 0.8))

  expect_equal(round(as.vector(q_DES)), c(224, 695, 1610))
  expect_equal(round(as.vector(q_rexp)), c(223, 691, 1606))

  # multiple events
  dag <- empty_dag() +
    node_td("Y", type="next_time", prob_fun=0.001)

  sim <- sim_discrete_event(dag, n_sim=1000000, max_t=Inf)
  #d_max_t <- sim[, .(max_t = max(start)), by=.id]
  #TODO: finish this

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

# TODO: check if
# - event_duration works
# - immunity_duration works
# - interrelated events work
# - using multiple unrelated time-dependent nodes is equal to just
#   calling rtexp() multiple times
# - check the arguments that are also used in sim_discrete_time()
# - what happens if censor_at_max_t and target_event are used at the same time?
