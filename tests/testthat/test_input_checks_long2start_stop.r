
test_that("data empty", {
  expect_error(long2start_stop(data=data.table(),
                               id=".id",
                               time=".time",
                               varying=c("A", "B")))
})

data <- data.table(id=c(1, 1, 2, 2),
                   time=c(1, 2, 1, 2),
                   A=c(TRUE, FALSE, TRUE, FALSE),
                   B=c(FALSE, TRUE, TRUE, TRUE),
                   C=c(1.2, 23.1, 1, 3))

test_that("wrong id", {
  expect_error(long2start_stop(data=data,
                               id=1,
                               time="time",
                               varying=c("A", "B")))
})

test_that("id not in data", {
  expect_error(long2start_stop(data=data,
                               id=".id",
                               time="time",
                               varying=c("A", "B")))
})

test_that("wrong id column type", {
  expect_error(long2start_stop(data=data,
                               id="A",
                               time="time",
                               varying=c("A", "B")))
})

test_that("wrong time", {
  expect_error(long2start_stop(data=data,
                               id="id",
                               time=2,
                               varying=c("A", "B")))
})

test_that("time not in data", {
  expect_error(long2start_stop(data=data,
                               id="id",
                               time=".time",
                               varying=c("A", "B")))
})

test_that("wrong time column type", {
  expect_error(long2start_stop(data=data,
                               id="id",
                               time="C",
                               varying=c("A", "B")))
})

test_that("wrong varying", {
  expect_error(long2start_stop(data=data,
                               id="id",
                               time="time",
                               varying=1))
})

test_that("varying not in data", {
  expect_error(long2start_stop(data=data,
                               id="id",
                               time="time",
                               varying=c("A", "D")))
})

test_that("no varying vars", {
  expect_warning(long2start_stop(data=data,
                                 id="id",
                                 time="time",
                                 varying=NULL))
})
