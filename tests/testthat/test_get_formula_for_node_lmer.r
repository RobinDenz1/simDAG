
test_that("general test case", {

  # a single random effect
  out <- get_formula_for_node_lmer(c("A", "B", "C"), "(1|D)")
  expect_equal(out, ...PLACEHOLDER... ~ A + B + C + (1 | D),
               ignore_formula_env=TRUE)

  # multiple random effects
  out <- get_formula_for_node_lmer(c("A", "B", "C"), c("(1|D)", "(G|F)"))
  expect_equal(out, ...PLACEHOLDER... ~ A + B + C + (1 | D) + (G | F),
               ignore_formula_env=TRUE)
})
