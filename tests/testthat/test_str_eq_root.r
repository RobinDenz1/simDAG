
test_that("normal distribution", {
  test_node <- node("test", type="rnorm", mean=10, sd=2)
  expect_equal(str_eq_root(test_node), "test ~ N(10, 2)")
})

test_that("bernoulli distribution", {
  test_node <- node("test", type="rbernoulli", p=0.6)
  expect_equal(str_eq_root(test_node), "test ~ Bernoulli(0.6)")
})

test_that("multinomial distribution", {
  test_node <- node("test", type="rcategorical", probs=c(0.1, 0.7, 0.2))
  expect_equal(str_eq_root(test_node),"test ~ Multinomial(c(0.1, 0.7, 0.2))")
})

test_that("using a constant", {
  test_node <- node("test", type="rconstant", constant=12)
  expect_equal(str_eq_root(test_node), "test ~ (12)")
})

test_that("using other root nodes", {
  test_node <- node("test", type="rbeta", shape1=2, shape2=0.2)
  expect_equal(str_eq_root(test_node), "test ~ rbeta(shape1=2, shape2=0.2)")
})
