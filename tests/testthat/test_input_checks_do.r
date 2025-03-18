
dag <- empty_dag() +
  node("A", type="rnorm", mean=23414, sd=234) +
  node("B", type="rcategorical") +
  node("C", type="binomial", parents=c("A", "B"))

test_that("not a DAG", {
  expect_error(do("", names="A", values=1))
})

test_that("wrong names", {
  expect_error(do(dag, names=1, values="a"))
})

test_that("names and values not same length", {
  expect_error(do(dag, names=c("A", "B"), values=1))
})
