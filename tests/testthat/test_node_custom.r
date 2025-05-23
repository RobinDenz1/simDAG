
test_that("multiple columns in custom root", {

  test_fun <- function(n) {
    out <- data.table(A=stats::rnorm(n),
                      B=stats::rnorm(n),
                      C=stats::rnorm(n))
    return(out)
  }

  dag <- empty_dag() +
    node("test", type=test_fun)
  data <- sim_from_dag(dag, n_sim=100)

  expect_true(ncol(data)==3)
  expect_true(nrow(data)==100)
  expect_equal(colnames(data), c("A", "B", "C"))
})

test_that("node needs intercept", {

  test_fun <- function(data, intercept) {
    return(data)
  }

  assign("test_fun", test_fun, envir=.GlobalEnv)

  expect_true(node_needs_intercept(test_fun))
  expect_true(node_needs_intercept("test_fun"))
})
