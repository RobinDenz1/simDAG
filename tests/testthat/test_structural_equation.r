
test_that("root node", {
  test_node <- node("A", type="rnorm", mean=10, sd=2)
  expect_equal(structural_equation(test_node), "A ~ N(10, 2)")
})

test_that("directly supported child node", {
  test_node <- node("A", type="gaussian", parents=c("C", "D"),
                    betas=c(0.1, 0.7), intercept=-0.3, error=2)
  expect_equal(structural_equation(test_node), "A ~ N(-0.3 + 0.1*C + 0.7*D, 2)")
})

test_that("custom child node", {
  node_fun <- function(data, arg1=32) {return(10)}
  assign("node_fun", value=node_fun, envir=.GlobalEnv)
  test_node <- node("A", type="fun", parents=c("C", "D"), arg1=33)
  expect_equal(structural_equation(test_node), "A ~ node_fun(C, D, arg1)")
})

test_that("directly supported time-varying node", {
  test_node <- node_td("A", type="time_to_event", prob_fun=0.02)
  expect_equal(structural_equation(test_node), "A(t) ~ Bernoulli(0.02)")
})

test_that("custom time-varying node", {
  node_testin <- function(data, sim_time, lmao=2){return(23)}
  assign("node_testin", value=node_testin, envir=.GlobalEnv)
  test_node <- node_td("A", type="testin", lmao=3)
  expect_equal(structural_equation(test_node), "A(t) ~ node_testin(t, lmao)")
})
