
test_that("only column names", {
  formula <- "~ -1 + A*2 + B*3 + C*3"
  expected <- list(formula_parts=c("A", "B", "C"),
                   mixed_terms=NULL,
                   betas=c(2, 3, 3),
                   intercept=-1)
  out <- parse_formula(formula, node_type="gaussian")
  expect_equal(out, expected)
})

test_that("no intercept with cox node", {
  formula <- "~ A*2 + B*3 + C*3"
  expected <- list(formula_parts=c("A", "B", "C"),
                   mixed_terms=NULL,
                   betas=c(2, 3, 3))
  out <- parse_formula(formula, node_type="cox")
  expect_equal(out, expected)
})

test_that("with cubic terms", {
  formula <- "~ -1 + A*2 + B*3 + C*3 + I(A^2)*-7"
  expected <- list(formula_parts=c("A", "B", "C", "I(A^2)"),
                   mixed_terms=NULL,
                   betas=c(2, 3, 3, -7),
                   intercept=-1)
  out <- parse_formula(formula, node_type="gaussian")
  expect_equal(out, expected)
})

test_that("with interactions", {
  formula <- "~ -1 + A*2 + B*3 + C*3 + A:C*0.3"
  expected <- list(formula_parts=c("A", "B", "C", "A:C"),
                   mixed_terms=NULL,
                   betas=c(2, 3, 3, 0.3),
                   intercept=-1)
  out <- parse_formula(formula, node_type="gaussian")
  expect_equal(out, expected)
})

test_that("with a missing intercept", {
  formula <- "~ A*2 + B*3 + C*3"
  expect_error(parse_formula(formula, node_type="gaussian"),
               "No intercept found in supplied 'formula'.")
})

test_that("intercept not a number", {
  formula <- "~ Alpha + A*2 + B*3 + C*3"
  expect_error(suppressWarnings(parse_formula(formula, node_type="gaussian")),
               paste0("One or more of the supplied beta coefficients ",
                      "in 'formula' or the intercept are not numbers."))
})

test_that("with a missing coefficient", {
  formula <- "~ 10 + A*2 + B*3 + C"
  expect_error(parse_formula(formula, node_type="gaussian"),
               paste0("Multiple intercepts or missing * found in 'formula': ",
                      "10, C. Please re-define the formula and re-run ",
                      "the function."), fixed=TRUE)
})

test_that("with a start too many", {
  formula <- "~ 10 + A*2 + B**3 + C*5"
  expect_error(parse_formula(formula, node_type="gaussian"),
               paste0("Missing variable name or coefficient in supplied ",
                      "'formula'. The problem starts somewhere at: 'B...'"),
               fixed=TRUE)
})

test_that("beta not a number", {
  formula <- "~ 5 + A*C + B*3 + C*3"
  expect_error(suppressWarnings(parse_formula(formula, node_type="gaussian")),
               paste0("One or more of the supplied beta coefficients ",
                      "in 'formula' or the intercept are not numbers."))
})

test_that("allows functions of numbers", {
  # one way
  formula <- "~ 3 + A*log(2) + B*exp(-1) + C*1.5"
  out <- parse_formula(formula, node_type="binomial")
  expect_equal(out$betas, c(log(2), exp(-1), 1.5))

  # other way
  formula <- "~ 3 + log(2)*A + exp(-1)*B + 1.5*C"
  out <- parse_formula(formula, node_type="binomial")
  expect_equal(out$betas, c(log(2), exp(-1), 1.5))
})

test_that("custom function intercept parsing", {

  test_fun1 <- function(data, parents, betas, intercept) {
    return(rep(0, nrow(data)))
  }

  test_fun2 <- function(data, parents, betas) {
    return(rep(0, nrow(data)))
  }

  # includes it if there and function needs it
  formula <- "~ 3 + A*log(2) + B*exp(-1) + C*1.5"
  out <- parse_formula(formula, node_type=test_fun1)
  expect_equal(out$intercept, 3)

  # excludes it if there and function does not need it
  out <- parse_formula(formula, node_type=test_fun2)
  expect_equal(out$intercept, NULL)

  # works if intercept is not there and function does not need it
  formula <- "~ A*log(2) + B*exp(-1) + C*1.5"
  out <- parse_formula(formula, node_type=test_fun2)
  expect_equal(out$intercept, NULL)

  # error if it is needed but not there
  expect_error(parse_formula(formula, node_type=test_fun1),
               "No intercept found in supplied 'formula'.")
})
