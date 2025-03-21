
test_that("only actual mixed model terms", {

  expected <- c("(1|D)", "(G|H)")

  # all at the end
  formstr <- "-2+A*3+B*-2+(1|D)+(G|H)"
  out <- extract_mixed_terms(formstr)
  expect_equal(out, expected)

  # some in the middle
  formstr <- "-2+A*3+(1|D)+B*-2+(G|H)"
  out <- extract_mixed_terms(formstr)
  expect_equal(out, expected)

  # some at the start
  formstr <- "(1|D)+-2+A*3+B*-2+(G|H)"
  out <- extract_mixed_terms(formstr)
  expect_equal(out, expected)
})

test_that("with additional cubic terms", {
  formstr <- "-2+A*3+B*-2+(1|D)+(G|H)+I(A^2)*0.3"
  out <- extract_mixed_terms(formstr)
  expect_equal(out, c("(1|D)", "(G|H)"))
})

test_that("with additional eval() terms", {
  formstr <- "-2+A*3+B*-2+(1|D)+(G|H)+eval(variable)*0.3"
  out <- extract_mixed_terms(formstr)
  expect_equal(out, c("(1|D)", "(G|H)"))
})
