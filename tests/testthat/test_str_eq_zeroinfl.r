
test_that("count formula, zero other", {
  test <- node("Y", type="zeroinfl",
               formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
               parents_zero=c("A", "B"),
               betas_zero=c(1, 2),
               intercept_zero=1,
               family_count="poisson")
  out <- str_eq_zeroinfl(test)
  expect_equal(out, c("Y[count] ~ Poisson(exp(-2 + A*0.2 + B*0.1 + A:B*0.4))",
                      "Y[zero] ~ Bernoulli(logit(1 + 1*A + 2*B))"))
})

test_that("zero formula, count other", {
  test <- node("Y", type="zeroinfl",
               parents_count=c("A", "B"),
               betas_count=c(0.2, 0.1),
               intercept_count=-2,
               formula_zero= ~ 1 + A*1 + B*2,
               family_count="poisson")
  out <- str_eq_zeroinfl(test)
  expect_equal(out, c("Y[count] ~ Poisson(exp(-2 + 0.2*A + 0.1*B))",
                      "Y[zero] ~ Bernoulli(logit(1 + A*1 + B*2))"))
})
