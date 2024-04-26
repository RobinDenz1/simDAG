
test_that("time_to_event, only with parents", {
  prob_A <- function(data){return(0.2)}
  test_node <- node_td("A", type="time_to_event", parents=c("C", "D"),
                       prob_fun=prob_A)
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Bernoulli(prob_A(C(t), D(t)))")
})

test_that("time_to_event, only with further arguments", {
  prob_A <- function(data, sim_time, rr_vacc=0.2){return(0.2)}
  test_node <- node_td("A", type="time_to_event", prob_fun=prob_A)
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Bernoulli(prob_A(t, rr_vacc))")
})

test_that("time_to_event, with parents and further arguments", {
  prob_A <- function(data, sim_time, rr_vacc=0.2){return(0.2)}
  test_node <- node_td("A", type="time_to_event", prob_fun=prob_A,
                       parents=c("C", "D"))
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Bernoulli(prob_A(t, rr_vacc, C(t), D(t)))")
})

test_that("competing_events, only with parents", {
  prob_A <- function(data){return(0.2)}
  test_node <- node_td("A", type="competing_events", parents=c("C", "D"),
                       prob_fun=prob_A)
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Multinomial(prob_A(C(t), D(t)))")
})

test_that("competing_events, only with further arguments", {
  prob_A <- function(data, sim_time, rr_vacc=0.2){return(0.2)}
  test_node <- node_td("A", type="competing_events", prob_fun=prob_A)
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Multinomial(prob_A(t, rr_vacc))")
})

test_that("competing_events, with parents and further arguments", {
  prob_A <- function(data, sim_time, rr_vacc=0.2){return(0.2)}
  test_node <- node_td("A", type="competing_events", prob_fun=prob_A,
                       parents=c("C", "D"))
  expect_equal(str_eq_time_to_event(test_node),
               "A(t) ~ Multinomial(prob_A(t, rr_vacc, C(t), D(t)))")
})
