
test_that("general test", {

  # NOTE: includes all 3 scenarios:
  # - only one event
  # - one event that goes on for too long
  # - multiple events

  data <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2,
                           3, 3, 3, 3, 3),
                     start=c(0, 10, 25, 50, 967,
                             0, 23, 56, 134,
                             0, 780, 981.2, 999, 1900.3),
                     stop=c(10, 25, 50, 967, NA,
                            23, 56, 134, NA,
                            780, 981.2, 999, 1900.3, NA),
                     Y=c(FALSE, TRUE, FALSE, TRUE, FALSE,
                         FALSE, FALSE, TRUE, FALSE,
                         FALSE, TRUE, TRUE, TRUE, TRUE))

  expected <- data.table(.id=c(1, 1, 1, 1, 1, 2, 2, 2, 2,
                               3, 3, 3, 3, 3),
                         start=c(0, 10, 25, 50, 967,
                                 0, 23, 56, 134,
                                 0, 780, 981.2, 999, 1900.3),
                         stop=c(10, 25, 50, 967, NA,
                                23, 56, 134, NA,
                                780, 981.2, 999, 1900.3, NA),
                         Y=c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE,
                             TRUE, FALSE, FALSE, TRUE, FALSE, FALSE,
                             FALSE, FALSE))

  set_event_centric(data, target_event="Y")

  expect_equal(data, expected)
})
