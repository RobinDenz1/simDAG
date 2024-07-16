
test_that("only column names", {
  formula <- "~ -1 + A*2 + B*3 + C*3"
  expected <- list(formula_parts=c("A", "B", "C"),
                   betas=c(2, 3, 3),
                   intercept=-1)
  out <- parse_formula(formula)
  expect_equal(out, expected)
})

test_that("with cubic terms", {
  formula <- "~ -1 + A*2 + B*3 + C*3 + I(A^2)*-7"
  expected <- list(formula_parts=c("A", "B", "C", "I(A^2)"),
                   betas=c(2, 3, 3, -7),
                   intercept=-1)
  out <- parse_formula(formula)
  expect_equal(out, expected)
})

test_that("with interactions", {
  formula <- "~ -1 + A*2 + B*3 + C*3 + A:C*0.3"
  expected <- list(formula_parts=c("A", "B", "C", "A:C"),
                   betas=c(2, 3, 3, 0.3),
                   intercept=-1)
  out <- parse_formula(formula)
  expect_equal(out, expected)
})

test_that("with a missing intercept", {
  formula <- "~ A*2 + B*3 + C*3"
  expect_error(parse_formula(formula),
               "No intercept found in supplied 'formula'.")
})

test_that("intercept not a number", {
  formula <- "~ Alpha + A*2 + B*3 + C*3"
  expect_error(suppressWarnings(parse_formula(formula)),
               paste0("Intercept supplied in 'formula' is not a number. ",
                      "Supplied intercept: Alpha"))
})

test_that("with a missing coefficient", {
  formula <- "~ 10 + A*2 + B*3 + C"
  expect_error(parse_formula(formula),
               paste0("Multiple intercepts or missing * found in 'formula': ",
                      "10, C. Please re-define the formula and re-run ",
                      "the function."), fixed=TRUE)
})

test_that("with a start too many", {
  formula <- "~ 10 + A*2 + B**3 + C*5"
  expect_error(parse_formula(formula),
               paste0("Missing variable name or coefficient in supplied ",
                      "'formula'. The problem starts somewhere at: 'B...'"),
               fixed=TRUE)
})

test_that("beta not a number", {
  formula <- "~ 5 + A*C + B*3 + C*3"
  expect_error(suppressWarnings(parse_formula(formula)),
               paste0("One or more of the supplied beta coefficients ",
                      "in 'formula' are not numbers."))
})
