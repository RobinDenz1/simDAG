
test_that("general test case", {
  formula <- "~ -10 + Alpha*2 + Beta*-0.3 + Alpha:Beta*0.11"
  args_orig <- list(formula=formula)
  args <- args_from_formula(args_orig, formula)
  expected <- list(parents=c("Alpha", "Beta", "Alpha:Beta"),
                   betas=c(2, -0.3, 0.11),
                   intercept=-10)
  expect_equal(args, expected)
})
