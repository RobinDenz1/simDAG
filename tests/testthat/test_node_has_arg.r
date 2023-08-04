
test_that("root node has the argument, no arg_is_true", {
  test <- node("test", type="rbernoulli", silly_arg="42")
  expect_true(node_has_arg(node=test, arg="silly_arg"))
})

test_that("root node has the argument, with arg_is_true, non-logical", {
  test <- node("test", type="rbernoulli", silly_arg="42")
  expect_false(node_has_arg(node=test, arg="silly_arg", arg_is_true=TRUE))
})

test_that("root node has the argument, with arg_is_true, logical", {
  test <- node("test", type="rbernoulli", silly_arg=TRUE)
  expect_true(node_has_arg(node=test, arg="silly_arg"))
})

test_that("root node doesn't have the argument", {
  test <- node("test", type="rbernoulli")
  expect_false(node_has_arg(node=test, arg="silly_arg"))
})

test_that("child node has the argument, no arg_is_true", {
  test <- node("test", type="binomial", parents="A", silly_arg="42",
               betas=1, intercept=1)
  expect_true(node_has_arg(node=test, arg="silly_arg"))
})

test_that("child node has the argument, with arg_is_true, non-logical", {
  test <- node("test", type="binomial", parents="A", silly_arg="42",
               betas=1, intercept=1)
  expect_false(node_has_arg(node=test, arg="silly_arg", arg_is_true=TRUE))
})

test_that("child node has the argument, with arg_is_true, logical", {
  test <- node("test", type="binomial", parents="A", silly_arg=TRUE,
               betas=1, intercept=1)
  expect_true(node_has_arg(node=test, arg="silly_arg"))
})

test_that("child node doesn't have the argument", {
  test <- node("test", type="binomial", parents="A")
  expect_false(node_has_arg(node=test, arg="silly_arg"))
})
