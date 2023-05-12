
#### testing root nodes output

test_that("root: all positional, no additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL)
  expect_equal(out, expected)
})

test_that("root: all positional, with additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list(p=0.5))
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL, p=0.5)
  expect_equal(out, expected)
})

test_that("root: no positional, no additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list())
  class(expected) <- "DAG.node"

  # specifying parents
  out <- node(name="C", type="rbernoulli", parents=NULL)
  expect_equal(out, expected)

  # not specifying parents
  out <- node(name="C", type="rbernoulli")
  expect_equal(out, expected)
})

test_that("root: no positional, with additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list(p=0.5))
  class(expected) <- "DAG.node"

  # specifying parents
  out <- node(name="C", parents=NULL, type="rbernoulli", p=0.5)
  expect_equal(out, expected)

  # not specifying parents
  out <- node(name="C", type="rbernoulli", p=0.5)
  expect_equal(out, expected)
})

test_that("root: name positional, rest named, no additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list())
  class(expected) <- "DAG.node"

  # specifying parents
  out <- node("C", parents=NULL, type="rbernoulli")
  expect_equal(out, expected)

  # not specifying parents
  out <- node("C", type="rbernoulli")
  expect_equal(out, expected)
})

test_that("root: name positional, rest named, with additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   params=list(p=0.5))
  class(expected) <- "DAG.node"

  # specifying parents
  out <- node("C", parents=NULL, type="rbernoulli", p=0.5)
  expect_equal(out, expected)

  # not specifying parents
  out <- node("C", type="rbernoulli", p=0.5)
  expect_equal(out, expected)
})


#### testing child nodes output

test_that("child: all positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", "binomial", c("A", "B"), betas=c(1, 2), intercept=-10,
              p=0.1)
  expect_equal(out, expected)
})

test_that("child: only name positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", type="binomial", parents=c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("child: name & type positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", "binomial", parents=c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("child: no positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node(name="C", type="binomial", parents=c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})
