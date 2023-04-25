
test_that("overall test case 1 time_to_event node", {
  # construct fake sim_discrete_time objects
  past_events_A <- list(1,
                        c(2, 4),
                        NULL, NULL, NULL, NULL,
                        3,
                        NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL,
                        c(5, 6, 7, 1, 8),
                        NULL, NULL, NULL, NULL,
                        9, NULL, NULL, NULL, NULL,
                        NULL, NULL,
                        c(2, 10, 4, 1),
                        NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL,
                        5)

  n_sim <- 11
  max_t <- 40

  sim <- list(max_t=max_t,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=5)),
              data=data.table(.id=seq(1, n_sim),
                              A_event=FALSE,
                              A_time=NA_integer_),
              tte_past_events=list(A=past_events_A))

  # expected output to faked sim object
  true_out <- data.table(
    .id=c(rep(1, 7), rep(2, 5), rep(3, 3), rep(4, 5),
          rep(5, 4), rep(6, 3), rep(7, 3), rep(8, 3),
          rep(9, 3), rep(10, 3), 11),
    start=c(0, 1, 6, 16, 21, 28, 33, 0, 2, 7, 28, 33,
            0, 7, 12, 0, 2, 7, 28, 33, 0, 16, 21, 40,
            0, 16, 21, 0, 16, 21, 0, 16, 21, 0, 21, 26,
            0, 28, 33, 0),
    stop=c(0, 5, 15, 20, 27, 32, 40, 1, 6, 27, 32, 40,
           6, 11, 40, 1, 6, 27, 32, 40, 15, 20, 39, 40,
           15, 20, 40, 15, 20, 40, 15, 20, 40, 20, 25, 40,
           27, 32, 40, 40),
    A=c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE,
        TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
        TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
        FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
        FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
        FALSE)
  )

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, true_out)
})

test_that("overall test case 3 time_to_event nodes", {
  past_events_A <- list(1, NULL, NULL, NULL, 2, NULL,
                        NULL, NULL, 2, NULL, NULL)
  past_events_B <- list(NULL, 1, NULL, NULL, NULL, 1,
                        NULL, NULL, NULL, c(2, 3), NULL)
  past_events_C <- list(NULL, NULL, NULL, NULL, NULL, 1,
                        NULL, 3, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5),
                            list(name="C",
                                 type="time_to_event",
                                 event_duration=15)),
              data=data.table(.id=c(1, 2, 3),
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_,
                              C_event=FALSE,
                              C_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B,
                                   C=past_events_C))

  # expected
  expected <- data.table(.id=c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
                         start=c(0, 1, 2, 6, 7, 11, 0, 5, 9, 10, 0, 8, 10),
                         stop=c(0, 1, 5, 6, 10, 11, 4, 8, 9, 11, 7, 9, 11),
                         A=c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
                             TRUE, TRUE, FALSE, FALSE, FALSE),
                         B=c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
                             FALSE, FALSE, TRUE, FALSE, FALSE, TRUE),
                         C=c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE,
                             FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("two events stopping at the same time", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, 1,
                        NULL, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  # expected
  expected <- data.table(.id=rep(1, 4), start=c(0, 1, 6, 11),
                         stop=c(0, 5, 10, 11),
                         A=c(FALSE, TRUE, TRUE, FALSE),
                         B=c(FALSE, FALSE, TRUE, FALSE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("two events starting at the same time", {
  past_events_A <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        1, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        1, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  # expected
  expected <- data.table(.id=rep(1, 2), start=c(0, 7),
                         stop=c(6, 11),
                         A=c(FALSE, TRUE),
                         B=c(FALSE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("one event starting when other ends", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        1, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  # expected
  expected <- data.table(.id=rep(1, 3), start=c(0, 1, 7),
                         stop=c(0, 6, 11),
                         A=c(FALSE, TRUE, FALSE),
                         B=c(FALSE, FALSE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("one event starting one t before other ends", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, 1,
                        NULL, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  # expected
  expected <- data.table(.id=rep(1, 4), start=c(0, 1, 6, 7),
                         stop=c(0, 5, 6, 11),
                         A=c(FALSE, TRUE, TRUE, FALSE),
                         B=c(FALSE, FALSE, TRUE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("event starting right when the same event just ended", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        1, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, 1)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  # expected (currently, not sure if I want it to stay like this)
  expected <- data.table(.id=rep(1, 4), start=c(0, 1, 7, 11),
                         stop=c(0, 6, 10, 11),
                         A=c(FALSE, TRUE, TRUE, TRUE),
                         B=c(FALSE, FALSE, FALSE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("node containing no events", {
  past_events_A <- list(NULL, NULL, NULL, 1, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  expected <- data.table(.id=rep(1, 3), start=c(0, 4, 10),
                         stop=c(3, 9, 11),
                         A=c(FALSE, TRUE, FALSE),
                         B=c(FALSE, FALSE, FALSE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("no events in any nodes", {
  past_events_A <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  expected <- data.table(.id=1, start=0, stop=11, A=FALSE, B=FALSE)

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

test_that("event at t = max_t", {
  past_events_A <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, 1)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=6),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=10)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  expected <- data.table(.id=c(1, 1), start=c(0, 11), stop=c(10, 11),
                         A=c(FALSE, FALSE), B=c(FALSE, TRUE))

  out_dat <- sim2start_stop.last(sim, include_tx_nodes=TRUE)

  expect_equal(out_dat, expected)
})

