
test_that("works with NULL input", {
  form <- NULL
  expect_equal(sanitize_formula(form), NULL)
})

test_that("works with normal formula", {
  form <- ~ A + B + I(A^3) + C*D
  out <- sanitize_formula(form)
  expect_equal(out, form)
})

test_that("works with special formula as string", {
  form <- "~ -5 + A*0.2 + B*0.5 + I(A^3)*-2 + C:D*0.2"
  out <- sanitize_formula(form)
  expect_equal(out, form)
})

test_that("works with special formula as formula", {
  form <- ~ -5 + A*0.2 + B*0.5 + I(A^3)*-2 + C:D*0.2
  out <- sanitize_formula(form)
  expect_equal(out, "~-5 + A * 0.2 + B * 0.5 + I(A^3) * -2 + C:D * 0.2")
})

test_that("works with some function calls in formula", {
  form <- ~ 3 + A*0.1 + B*log(2) + I(A^3)*0.2 + C:D*exp(-4)
  out <- sanitize_formula(form)
  expect_true(is.character(out))
})

test_that("works with all betas being function calls in formula", {
  # one way
  form <- ~ 3 + A*exp(0.1) + B*log(2)
  out <- sanitize_formula(form)
  expect_true(is.character(out))

  # other way
  form <- ~ 3 + exp(0.1)*A + log(2)*B
  out <- sanitize_formula(form)
  expect_true(is.character(out))
})
