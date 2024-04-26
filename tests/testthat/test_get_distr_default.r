
test_that("defaults correct", {
  expect_equal(get_distr_default("rnorm"), "N")
  expect_equal(get_distr_default("rbernoulli"), "Bernoulli")
  expect_equal(get_distr_default("rcategorical"), "Multinomial")
  expect_equal(get_distr_default("rconstant"), "")
  expect_equal(get_distr_default("rbeta"), "rbeta")
})
