
test_that("regular number", {
  expect_true(is_valid_number("10"))
})

test_that("decimal number", {
  expect_true(is_valid_number("10.1"))
})

test_that("negative number", {
  expect_true(is_valid_number("-10"))
})

test_that("function of number", {
  expect_true(is_valid_number("log(10)"))
})

test_that("function of negative number", {
  expect_true(is_valid_number("log(-19)"))
  expect_true(is_valid_number("log10(-19)"))
  expect_true(is_valid_number("log_2(-19)"))
  expect_true(is_valid_number("log.2(-19)"))
})

test_that("eval call", {
  expect_true(is_valid_number("eval(beta)"))
  expect_true(is_valid_number("eval(beta.var)"))
  expect_true(is_valid_number("eval(beta_var)"))
  expect_true(is_valid_number("eval(beta_var2)"))
})

test_that("not a number", {
  expect_false(is_valid_number("AVC"))
  expect_false(is_valid_number("A.VC.4"))
  expect_false(is_valid_number("A_VC"))
  expect_false(is_valid_number("A34VC"))
})
