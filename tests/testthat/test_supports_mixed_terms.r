
test_that("works with function input", {
  expect_true(supports_mixed_terms(node_gaussian))
  expect_true(supports_mixed_terms(node_binomial))
  expect_true(supports_mixed_terms(node_poisson))
  expect_true(!supports_mixed_terms(node_negative_binomial))
  expect_true(!supports_mixed_terms(substr))
})

test_that("works with string input", {
  expect_true(supports_mixed_terms("gaussian"))
  expect_true(supports_mixed_terms("binomial"))
  expect_true(supports_mixed_terms("poisson"))
  expect_true(!supports_mixed_terms("negative_binomial"))
  expect_true(!supports_mixed_terms("substr"))
})
