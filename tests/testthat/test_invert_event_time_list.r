
past_events_list <- list(A=list(c(1, 4, 6),
                                c(2),
                                c(5, 3)),
                         B=list(numeric(0),
                                c(1, 2, 3, 4),
                                5))

true_inv_A <- list(1, 2, 3, 1, 3, 1)
true_inv_B <- list(2, 2, 2, 2, 3, NULL)

test_that("example case working", {
  inv_A <- invert_event_time_list(tte_list=past_events_list[[1]], n_sim=6)
  inv_B <- invert_event_time_list(tte_list=past_events_list[[2]], n_sim=6)

  expect_equal(inv_A, true_inv_A)
  expect_equal(inv_B, true_inv_B)
})

test_that("works with empty list", {
  inv <- invert_event_time_list(list(), n_sim=5)
  true_inv <- list(NULL, NULL, NULL, NULL, NULL)

  expect_equal(inv, true_inv)
})
