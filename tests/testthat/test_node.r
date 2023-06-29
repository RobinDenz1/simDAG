
#### testing root nodes output

test_that("root: all positional, no additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL)
  expect_equal(out, expected)
})

test_that("root: all positional, with additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   time_varying=FALSE,
                   params=list(p=0.5))
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL, p=0.5)
  expect_equal(out, expected)
})

test_that("root: no positional, no additional", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   time_varying=FALSE,
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
                   time_varying=FALSE,
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
                   time_varying=FALSE,
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
                   time_varying=FALSE,
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
                   time_varying=FALSE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", "binomial", c("A", "B"), betas=c(1, 2), intercept=-10,
              p=0.1)
  expect_equal(out, expected)
})

test_that("child: all positional with formula", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   formula=~ A + B,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", "binomial", NULL, ~ A + B, betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("child: only name positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", type="binomial", parents=c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("child: only name positional with formula", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   formula=~ A + B + I(A^2),
                   betas=c(1, 2, 3),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", type="binomial", parents=c("A", "B"),
              formula=~ A + B + I(A^2), betas=c(1, 2, 3),
              intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("child: name & type positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=FALSE,
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
                   time_varying=FALSE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node(name="C", type="binomial", parents=c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("child: no positional with formula", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   formula=~ A + B + I(A^2),
                   betas=c(1, 2, 3),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node(name="C", type="binomial", parents=NULL,
              formula=~ A + B + I(A^2), betas=c(1, 2, 3),
              intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

#### testing child nodes output with time-varying=TRUE

test_that("time-varying: all positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=TRUE,
                   formula= ~ A + B,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node_td("C", "binomial", c("A", "B"), ~ A + B,
                 betas=c(1, 2), intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("time-varying: only name positional", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=TRUE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node_td("C", type="binomial", parents=c("A", "B"), betas=c(1, 2),
                 intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("time-varying: only name positional with formula", {
  expected <- list(name="C",
                   type="binomial",
                   parents=c("A", "B"),
                   time_varying=TRUE,
                   formula=~ A + B + I(A^2),
                   betas=c(1, 2, 3),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node_td("C", type="binomial", parents=c("A", "B"),
                 formula=~ A + B + I(A^2), betas=c(1, 2, 3),
                 intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("call with only two unnamed arguments", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli")
  expect_equal(out, expected)
})

test_that("call with only two named arguments", {
  expected <- list(name="C",
                   type="rbernoulli",
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node(type="rbernoulli", name="C")
  expect_equal(out, expected)
})

test_that("error when name or type missing", {
  expect_error(empty_dag() + node())
  expect_error(empty_dag() + node(name="A"))
  expect_error(empty_dag() + node(type="rbernoulli"))
  expect_error(empty_dag() + node(name="A", betas=c(1, 2), intercept=2,
                                  parents=c("sex", "age"), error=2))
})
