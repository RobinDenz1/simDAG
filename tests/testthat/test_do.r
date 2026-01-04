
test_that("do on one node", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2), intercept=2,
         error=10)

  out <- do(dag, names="A", values=1)

  expected <- empty_dag() +
    node("A", type="rconstant", constant=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2), intercept=2,
         error=10)

  expect_equal(out, expected)
})

test_that("do multiple nodes", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2), intercept=2,
         error=10)

  out <- do(dag, names=c("A", "B", "C"), values=c(1, 2, 3))

  expected <- empty_dag() +
    node("A", type="rconstant", constant=1) +
    node("B", type="rconstant", constant=2) +
    node("C", type="rconstant", constant=3)

  expect_equal(out, expected)
})

test_that("with time-dependent node", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node_td("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2),
            intercept=2, error=10)

  out <- do(dag, names="C", values=1)

  expected <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node("C", type="rconstant", constant=1)

  expect_equal(out, expected)
})

test_that("replace nodes with other node definitions", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node_td("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2),
            intercept=2, error=10)

  dag2 <- do(dag, names="B", values=list(
    node(".", type="gaussian", formula= ~ -2 + A*3, error=1)
  ))

  dag_expected <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node_td("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2),
            intercept=2, error=10) +
    node("B", type="gaussian", formula= ~ -2 + A*3, error=1)

  expect_equal(dag2, dag_expected)
})

test_that("mix of fixed values and node definitions in values argument", {

  dag <- empty_dag() +
    node("A", type="rnorm", mean=10, sd=1) +
    node("B", type="binomial", parents="A", betas=3, intercept=-1) +
    node_td("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2),
            intercept=2, error=10)

  dag2 <- do(dag, names=c("B", "A"), values=list(
    node(".", type="gaussian", formula= ~ -2 + A*3, error=1),
    10
  ))

  dag_expected <- empty_dag() +
    node("A", type="rconstant", constant=10) +
    node_td("C", type="gaussian", parents=c("A", "B"), betas=c(1, 2),
            intercept=2, error=10) +
    node("B", type="gaussian", formula= ~ -2 + A*3, error=1)

  expect_equal(dag2, dag_expected)
})
