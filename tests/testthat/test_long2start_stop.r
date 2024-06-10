
test_that("general test case", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                             TRUE),
                     B=FALSE)
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4),
                         stop=c(rep(5, 8), 3, 5, 3, 5),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE),
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("shuffled input", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=FALSE)
  long <- long[sample(nrow(long), replace=FALSE),]

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4),
                         stop=c(rep(5, 8), 3, 5, 3, 5),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE),
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("event right at the end", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=c(rep(FALSE, 49), TRUE))
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4, 5),
                         stop=c(rep(5, 8), 3, 5, 3, 4, 5),
                         A=c(rep(FALSE, 9), TRUE, FALSE, TRUE, TRUE),
                         B=c(rep(FALSE, 12), TRUE))

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("non-logical time-varying variables", {

  long <- data.table(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(13.2, 43), 14.75, 14.75, rep(18, 3), 27.1,
                         53),
                     B=14.3)
  setkey(long, .id, .simulation_time)

  expected <- data.table(.id=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 10, 10),
                         start=c(rep(1, 9), 4, 1, 4, 5),
                         stop=c(rep(5, 8), 3, 5, 3, 4, 5),
                         A=c(rep(13.2, 9), 14.75, 18, 27.1, 53),
                         B=14.3)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=c("A", "B"))

  expect_equal(out_dat, expected)
})

test_that("no varying variables", {

  long <- data.frame(.id=rep(seq_len(10), each=5),
                     .simulation_time=rep(seq_len(5), 10),
                     A=c(rep(FALSE, 43), TRUE, TRUE, rep(FALSE, 3), TRUE,
                         TRUE),
                     B=FALSE)

  expected <- data.table(.id=seq_len(10),
                         start=1,
                         stop=5,
                         A=FALSE,
                         B=FALSE)

  out_dat <- long2start_stop(data=long, id=".id", time=".simulation_time",
                             varying=NULL, check_inputs=FALSE)

  expect_equal(out_dat, expected)
})

test_that("wrong data", {
  expect_error(long2start_stop(data="1", id=".id", time=".time"))
})
