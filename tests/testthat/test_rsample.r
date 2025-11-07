
test_that("no replacement", {
  set.seed(1234)
  dag <- empty_dag() +
    node("mpg", type="rsample", x=mtcars$mpg, replace=FALSE)
  data <- sim_from_dag(dag, n_sim=10)
  expect_equal(round(mean(data$mpg), 3), 19.14)
})

test_that("with replacement", {
  set.seed(123445)
  dag <- empty_dag() +
    node("mpg", type="rsample", x=mtcars$mpg, replace=TRUE)
  data <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(data$mpg), 3), 20.046)
})

test_that("with probs", {
  set.seed(123434)
  dag <- empty_dag() +
    node("A", type="rsample", x=c(1, 2, 3, 4), replace=TRUE,
         prob=c(0.1, 0.3, 0.1, 0.5))
  data <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(data$A), 3), 3.003)
})
