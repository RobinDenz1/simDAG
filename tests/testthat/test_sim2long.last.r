
test_that("general test case", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, 1,
                        NULL, NULL, NULL, NULL, NULL)

  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10,
                                 time_varying=TRUE),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5,
                                 time_varying=TRUE)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  expected <- data.table(.id=rep(1, 11),
                         .time=seq_len(11),
                         A=c(rep(TRUE, 10), FALSE),
                         B=c(rep(FALSE, 5), rep(TRUE, 5), FALSE))
  setkey(expected, .id, .time)

  out_dat <- sim2long.last(sim)

  expect_equal(out_dat, expected)
})

test_that("adding time_since_last and event_count afterwards", {
  past_events_A <- list(1, NULL, NULL, NULL, NULL, NULL,
                        NULL, NULL, NULL, NULL, NULL)
  past_events_B <- list(NULL, NULL, NULL, NULL, NULL, 1,
                        NULL, NULL, NULL, NULL, NULL)

  # NOTE: what if node_td has no parents?
  sim <- list(max_t=11,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10,
                                 time_since_last=TRUE,
                                 time_varying=TRUE),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5,
                                 event_count=TRUE,
                                 time_since_last=TRUE,
                                 time_varying=TRUE)),
              data=data.table(.id=1,
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=past_events_A, B=past_events_B))

  expected <- data.table(.id=rep(1, 11),
                         .time=seq_len(11),
                         A=c(rep(TRUE, 10), FALSE),
                         B=c(rep(FALSE, 5), rep(TRUE, 5), FALSE),
                         B_event_count=c(rep(0, 5), rep(1, 6)),
                         A_time_since_last=seq(0, 10, 1),
                         B_time_since_last=c(rep(NA, 5), seq(0, 5, 1)))
  setkey(expected, .id, .time)

  out_dat <- sim2long.last(sim)

  expect_equal(out_dat, expected)
})
