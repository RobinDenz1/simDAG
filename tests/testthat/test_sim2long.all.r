
test_that("general test case", {

  sim <- list(max_t=5,
              tx_nodes=list(list(name="A",
                                 type="time_to_event",
                                 event_duration=10),
                            list(name="B",
                                 type="time_to_event",
                                 event_duration=5)),
              data=data.table(.id=seq(1, 10),
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=list(), B=list()),
              past_states=list(data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=c(rep(FALSE, 8), TRUE, TRUE),
                                          A_time=c(rep(NA_integer_, 8), 4, 4),
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=c(rep(FALSE, 8), TRUE, TRUE),
                                          A_time=c(rep(NA_integer_, 8), 4, 4),
                                          B_event=FALSE,
                                          B_time=NA_integer_)))

  expected <- data.table(.id=rep(seq_len(10), each=5),
                         .time=rep(seq_len(5), 10),
                         A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                             TRUE),
                         B=FALSE)
  setkey(expected, .id, .time)

  out_dat <- sim2long.all(sim)

  expect_equal(out_dat, expected)
})

test_that("test with competing events nodes", {

  sim <- list(max_t=5,
              tx_nodes=list(list(name="A",
                                 type="competing_events",
                                 event_duration=10),
                            list(name="B",
                                 type="competing_events",
                                 event_duration=5)),
              data=data.table(.id=seq(1, 10),
                              A_event=FALSE,
                              A_time=NA_integer_,
                              B_event=FALSE,
                              B_time=NA_integer_),
              tte_past_events=list(A=list(), B=list()),
              past_states=list(data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=FALSE,
                                          A_time=NA_integer_,
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=c(rep(FALSE, 8), TRUE, TRUE),
                                          A_time=c(rep(NA_integer_, 8), 4, 4),
                                          B_event=FALSE,
                                          B_time=NA_integer_),
                               data.table(.id=seq(1, 10),
                                          A_event=c(rep(FALSE, 8), TRUE, TRUE),
                                          A_time=c(rep(NA_integer_, 8), 4, 4),
                                          B_event=FALSE,
                                          B_time=NA_integer_)))

  expected <- data.table(.id=rep(seq_len(10), each=5),
                         .time=rep(seq_len(5), 10),
                         A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                             TRUE),
                         B=FALSE)
  setkey(expected, .id, .time)

  out_dat <- sim2long.all(sim)

  expect_equal(out_dat, expected)
})
