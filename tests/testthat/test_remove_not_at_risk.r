
test_that("general test case, no overlap", {

  data <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3),
                     start=c(1, 3, 20, 30, 80, 1, 3, 44, 1, 100),
                     stop=c(2, 19, 29, 79, 100, 2, 43, 120, 99, 108),
                     event=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
                             FALSE, TRUE, FALSE))

  expected <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 3),
                         start=c(1, 3, 39, 80, 1, 22, 44, 1),
                         stop=c(2, 19, 79, 100, 2, 43, 120, 99),
                         event=c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
                                 FALSE, TRUE))

  out <- remove_not_at_risk(data=data, duration=20, target_event="event",
                            overlap=FALSE)

  expect_equal(out, expected)
})

test_that("general test case, with overlap", {

  data <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3),
                     start=c(1, 3, 20, 30, 80, 1, 3, 44, 1, 100),
                     stop=c(2, 19, 29, 79, 100, 2, 43, 120, 99, 108),
                     event=c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
                             FALSE, TRUE, FALSE))
  data[, stop := stop + 1]

  expected <- data.table(.id=c(1, 1, 1, 1, 2, 2, 2, 3),
                         start=c(1, 3, 39, 80, 1, 22, 44, 1),
                         stop=c(2, 19, 79, 100, 2, 43, 120, 99),
                         event=c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
                                 FALSE, TRUE))
  expected[, stop := stop + 1]

  out <- remove_not_at_risk(data=data, duration=20, target_event="event",
                            overlap=TRUE)

  expect_equal(out, expected)
})

test_that("events right after each other", {

  input <- data.table(.id=c(1, 1, 1, 2, 2, 2, 2),
                      start=c(1, 15, 45, 1, 3, 44, 201),
                      stop=c(14, 44, 109, 2, 43, 200, 3124),
                      sickness=c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE))

  expected <- data.table(.id=c(1, 1, 1, 2, 2, 2, 2),
                         start=c(1, 26, 56, 1, 3, 55, 201),
                         stop=c(14, 44, 109, 2, 43, 200, 3124),
                         sickness=c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE,
                                    TRUE))

  out <- remove_not_at_risk(data=input, duration=12, target_event="sickness",
                            overlap=FALSE)

  expect_equal(out, expected)
})
