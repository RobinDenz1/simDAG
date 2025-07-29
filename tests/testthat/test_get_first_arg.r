
test_that("general test case", {
  out <- get_first_arg("net(mean(A))")
  expect_equal(out, "mean(A)")
})

test_that("with second argument", {
  out <- get_first_arg("net(sum(A), net='something')")
  expect_equal(out, "sum(A)")
})

test_that("with complicated nested functions", {
  out <- get_first_arg("net((as.numeric(any(A/2))) > 2)")
  expect_equal(out, "(as.numeric(any(A/2))) > 2")
})

test_that("with complicated nested functions and second argument", {
  out <- get_first_arg("net((as.numeric(any(A/2))) > 2, net='network10')")
  expect_equal(out, "(as.numeric(any(A/2))) > 2")
})

test_that("with comma in first argument", {
  out <- get_first_arg("net(sum(A, na.rm=TRUE))")
  expect_equal(out, "sum(A, na.rm=TRUE)")
})

test_that("with complicated variable name", {
  out <- get_first_arg("net(mean(head(`some-variable^3`)))")
  expect_equal(out, "mean(head(`some-variable^3`))")
})
