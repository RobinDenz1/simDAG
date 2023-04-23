
vec <- c(1, NA, NA, NA, 3, NA, NA, 4, NA, 6)
true_filled <- c(1, 1, 1, 1, 3, 3, 3, 4, 4, 6)

test_that("example case working", {
  filled <- na_locf(vec)
  expect_equal(filled, true_filled)
})

test_that("works with no missings", {
  expect_equal(na_locf(true_filled), true_filled)
})

test_that("works with empty vector", {
  filled <- na_locf(numeric(0))
  expect_equal(filled, numeric(0))
})
