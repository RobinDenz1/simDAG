
test_that("name root node", {
  expect_error(node(c("A", "B"), type="rbernoulli"))
})

test_that("name root node", {
  expect_error(node("A", type="something"))
})

test_that("name child node", {
  expect_error(node(c("A", "B"), type="binomial", parents="C",
                    betas=1))
})

test_that("type child node", {
  expect_error(node("A", type="rbernoulli", parents="C",
                    betas=1))
})

test_that("parents child node", {
  expect_error(node("A", type="binomial", parents=list("A", "B"), betas=1))
})
