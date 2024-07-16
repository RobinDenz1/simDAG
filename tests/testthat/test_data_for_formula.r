
dag <- empty_dag() +
  node("A", type="rnorm") +
  node(c("B", "B_clone"), type="rbernoulli") +
  node("test", type="rcategorical", probs=c(0.1, 0.2, 0.2, 0.5),
       coerce2factor=TRUE) +
  node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 2),
       error=0.001, intercept=-2)

set.seed(34)
data <- sim_from_dag(dag, n_sim=300)

test_that("only variable names", {
  args <- list(parents=c("A", "C"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "C"))
})

test_that("with categorical covariate", {
  args <- list(parents=c("A", "C", "test1", "test2"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "C", "test1", "test2"))
})

test_that("with logical covariate", {
  args <- list(parents=c("A", "BTRUE"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "BTRUE"))
})

test_that("with interaction between two continuous variables", {
  args <- list(parents=c("A", "C", "A:C"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "C", "A:C"))
})

test_that("with interaction between continuous and categorical variable", {
  args <- list(parents=c("A", "C", "test1", "test2", "A:test1"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "C", "test1", "test2", "A:test1"))
})

test_that("with interaction between two categorical variable", {
  args <- list(parents=c("A", "C", "test1", "test2", "BTRUE:test1"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "C", "test1", "test2", "BTRUE:test1"))
})

test_that("with cubic term", {
  args <- list(parents=c("A", "I(A^14)"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "I(A^14)"))
})

test_that("with higher order interactions", {
  args <- list(parents=c("A", "BTRUE:B_cloneTRUE:test1"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), c("A", "BTRUE:B_cloneTRUE:test1"))
})

test_that("with everything", {
  args <- list(parents=c("A", "BTRUE", "test2", "I(A^14)",
                         "A:C", "test3:C", "BTRUE:test2",
                         "BTRUE:B_cloneTRUE:test1"))

  out <- data_for_formula(data, args)
  expect_equal(colnames(out), args$parents)
})

test_that("useful error message", {
  args <- list(parents=c("A", "C", "test1", "test2", "test1:A"))

  expect_error(data_for_formula(data, args))
})
