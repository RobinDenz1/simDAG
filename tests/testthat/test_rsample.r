
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

test_that("with data.frame like object in x", {

  set.seed(1234)

  test_data <- data.frame(A=stats::rnorm(100),
                          B=rbernoulli(100),
                          C=rcategorical(100, c(0.1, 0.5, 0.4)))

  dag <- empty_dag() +
    node("X", type="rbernoulli") +
    node("placeholder", type="rsample", x=test_data, replace=FALSE) +
    node("Y", type="binomial", formula= ~ -1 + A*2 + B*-0.7 + X*0.4)

  data <- sim_from_dag(dag, n_sim=50)

  expect_equal(nrow(data), 50)
  expect_equal(ncol(data), 5)
  expect_equal(colnames(data), c("X", "A", "B", "C", "Y"))

  # equivalent results with data.table
  dag <- empty_dag() +
    node("X", type="rbernoulli") +
    node("placeholder", type="rsample", x=as.data.table(test_data),
         replace=FALSE) +
    node("Y", type="binomial", formula= ~ -1 + A*2 + B*-0.7 + X*0.4)

  data <- sim_from_dag(dag, n_sim=50)

  expect_equal(nrow(data), 50)
  expect_equal(ncol(data), 5)
  expect_equal(colnames(data), c("X", "A", "B", "C", "Y"))

  # if variables already exist, they will be automatically renamed
  dag <- empty_dag() +
    node("A", type="rbernoulli") +
    node("placeholder", type="rsample", x=test_data) +
    node("Y", type="binomial", formula= ~ -1 + ATRUE*2 + BTRUE*-0.7 + C*0.4)

  data <- sim_from_dag(dag, n_sim=50)

  expect_equal(nrow(data), 50)
  expect_equal(ncol(data), 5)
  expect_equal(colnames(data), c("A", "A", "B", "C", "Y"))
})
