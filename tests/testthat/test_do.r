
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
