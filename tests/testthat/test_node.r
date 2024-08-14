
#### testing root nodes output

test_that("root: accepts built-in function in type", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", rbernoulli, NULL)
  expect_equal(out, expected)
})

test_that("root: accepts custom function in type", {

  rtesting <- function(n) {
    return(n)
  }

  expected <- list(name="C",
                   type_str="rtesting",
                   type_fun=rtesting,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", rtesting, NULL)
  expect_equal(out, expected)
})

test_that("root: all positional, no additional", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL)
  expect_equal(out, expected)
})

test_that("root: multiple names, all positional, no additional", {
  expected <- list(list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list()),
                   list(name="D",
                        type_str="rbernoulli",
                        type_fun=rbernoulli,
                        parents=NULL,
                        time_varying=FALSE,
                        params=list()))
  class(expected[[1]]) <- "DAG.node"
  class(expected[[2]]) <- "DAG.node"
  class(expected) <- "DAG.node"

  out <- node(c("C", "D"), "rbernoulli", NULL)
  expect_equal(out, expected)
})

test_that("root: all positional, with additional", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list(p=0.5))
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli", NULL, p=0.5)
  expect_equal(out, expected)
})

test_that("root: no positional, no additional", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
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
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
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
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
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
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
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

test_that("child: accepts built-in function in type", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", node_binomial, c("A", "B"), betas=c(1, 2), intercept=-10,
              p=0.1)
  expect_equal(out, expected)
})

test_that("child: accepts custom function in type", {

  some_test_fun <- function(data, parents) {
    return("ay")
  }

  expected <- list(name="C",
                   type_str="some_test_fun",
                   type_fun=some_test_fun,
                   parents=c("A", "B"),
                   time_varying=FALSE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node("C", some_test_fun, c("A", "B"), betas=c(1, 2), intercept=-10,
              p=0.1)
  expect_equal(out, expected)
})

test_that("child: all positional", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
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

test_that("child: multiple, all positional", {
  expected <- list(list(name="C",
                        type_str="binomial",
                        type_fun=node_binomial,
                        parents=c("A", "B"),
                        time_varying=FALSE,
                        betas=c(1, 2),
                        intercept=-10,
                        p=0.1),
                   list(name="D",
                        type_str="binomial",
                        type_fun=node_binomial,
                        parents=c("A", "B"),
                        time_varying=FALSE,
                        betas=c(1, 2),
                        intercept=-10,
                        p=0.1))
  class(expected[[1]]) <- "DAG.node"
  class(expected[[2]]) <- "DAG.node"
  class(expected) <- "DAG.node"

  out <- node(c("C", "D"), "binomial", c("A", "B"), betas=c(1, 2),
              intercept=-10, p=0.1)
  expect_equal(out, expected)
})

test_that("child: all positional with formula", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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

test_that("time-varying: accepts built-in fun in type", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
                   parents=c("A", "B"),
                   time_varying=TRUE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node_td("C", node_binomial, c("A", "B"), NULL,
                 betas=c(1, 2), intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("time-varying: accepts custom fun in type", {

  custom_fun <- function(data, parents) {
    return("ahh")
  }

  expected <- list(name="C",
                   type_str="custom_fun",
                   type_fun=custom_fun,
                   parents=c("A", "B"),
                   time_varying=TRUE,
                   betas=c(1, 2),
                   intercept=-10,
                   p=0.1)
  class(expected) <- "DAG.node"

  out <- node_td("C", custom_fun, c("A", "B"), NULL,
                 betas=c(1, 2), intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("time-varying: all positional", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
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

test_that("time-varying: multiple, all positional", {
  expected <- list(list(name="C",
                        type_str="binomial",
                        type_fun=node_binomial,
                        parents=c("A", "B"),
                        time_varying=TRUE,
                        formula= ~ A + B,
                        betas=c(1, 2),
                        intercept=-10,
                        p=0.1),
                   list(name="D",
                        type_str="binomial",
                        type_fun=node_binomial,
                        parents=c("A", "B"),
                        time_varying=TRUE,
                        formula= ~ A + B,
                        betas=c(1, 2),
                        intercept=-10,
                        p=0.1))
  class(expected[[1]]) <- "DAG.node"
  class(expected[[2]]) <- "DAG.node"
  class(expected) <- "DAG.node"

  out <- node_td(c("C", "D"), "binomial", c("A", "B"), ~ A + B,
                 betas=c(1, 2), intercept=-10, p=0.1)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("time-varying: only name positional", {
  expected <- list(name="C",
                   type_str="binomial",
                   type_fun=node_binomial,
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
                   type_str="binomial",
                   type_fun=node_binomial,
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

test_that("time-varying: no parents", {
  expected <- list(name="C",
                   type_str="time_to_event",
                   type_fun=node_time_to_event,
                   parents=NULL,
                   time_varying=TRUE,
                   prob_fun=0.001)
  class(expected) <- "DAG.node"

  out <- node_td("C", "time_to_event", prob_fun=0.001)
  expect_equal(out, expected, ignore_formula_env=TRUE)
})

test_that("call with only two unnamed arguments", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node("C", "rbernoulli")
  expect_equal(out, expected)
})

test_that("call with only two named arguments", {
  expected <- list(name="C",
                   type_str="rbernoulli",
                   type_fun=rbernoulli,
                   parents=NULL,
                   time_varying=FALSE,
                   params=list())
  class(expected) <- "DAG.node"

  out <- node(type="rbernoulli", name="C")
  expect_equal(out, expected)
})

test_that("cubic terms showing up in parents", {
  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node("C", type="gaussian", formula= ~ -2 + A*1 + I(B^2)*0.3, error=2)

  expect_equal(dag$child_nodes[[1]]$parents, c("A", "B"))
})

test_that("node: error when name or type missing", {
  expect_error(empty_dag() + node())
  expect_error(empty_dag() + node(name="A"))
  expect_error(empty_dag() + node(type="rbernoulli"))
  expect_error(empty_dag() + node(name="A", betas=c(1, 2), intercept=2,
                                  parents=c("sex", "age"), error=2))
})

test_that("node_td: error when name or type missing", {
  expect_error(empty_dag() + node_td())
  expect_error(empty_dag() + node_td(name="A"))
  expect_error(empty_dag() + node_td(type="rbernoulli"))
  expect_error(empty_dag() + node_td(name="A", betas=c(1, 2), intercept=2,
                                     parents=c("sex", "age"), error=2))
})

test_that("snapshot print node_td", {
  expect_snapshot_output(print(node_td("A", type="time_to_event")))
})
