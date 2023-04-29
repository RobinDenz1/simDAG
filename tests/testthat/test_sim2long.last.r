
test_that("general test case", {
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

  expected <- data.table(.id=rep(1, 11),
                         .time=seq_len(11),
                         A=c(rep(TRUE, 10), FALSE),
                         B=c(rep(FALSE, 5), rep(TRUE, 5), FALSE))

  out_dat <- sim2long.last(sim)

  expect_equal(out_dat, expected)
})
