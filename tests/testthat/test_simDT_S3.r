
fake_simDT <- list(data=data.table(.id=seq_len(15),
                                   A=rnorm(15)),
                   max_t=50,
                   tx_nodes=list(A=list(), B=list(), C=list()),
                   tte_past_events=list(A=list(), B=list(), C=list()),
                   ce_past_events=list(),
                   save_states="all")
class(fake_simDT) <- "simDT"

test_that("print.simDT, save_states='all'", {
  expect_snapshot_output(print(fake_simDT))
})

test_that("print.simDT, save_states='last'", {
  fake_simDT$save_states <- "last"
  expect_snapshot_output(print(fake_simDT))
})

test_that("print.simDT, save_states='at_t'", {
  fake_simDT$save_states <- "at_t"
  expect_snapshot_output(print(fake_simDT))
})

test_that("summary.simDT", {
  expect_snapshot_output(summary(fake_simDT))
})

test_that("error when x not simDT", {
  expect_error(plot.simDT("10"))
})
