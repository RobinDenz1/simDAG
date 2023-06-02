
test_that("one value", {
  out <- rconstant(n=1, 10)
  expect_equal(out, 10)

  out <- rconstant(n=1, "BLA")
  expect_equal(out, "BLA")
})

test_that("multiple values", {
  out <- rconstant(n=10, 10)
  expect_equal(out, rep(10, 10))

  out <- rconstant(n=10, "BLA")
  expect_equal(out, rep("BLA", 10))
})
