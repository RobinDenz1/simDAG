
test_that("general test case", {

  test <- node(
    "Y", type="mixture", parents="A",
    distr=list(
      "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
      "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1)
  ))

  out <- str_eq_mixture(test)
  expect_equal(out, c("Y[A==0] ~ N(-2 + B*2, 1)", "Y[A==1] ~ N(3 + B*5, 1)"))
})

test_that("including nodes with multiple parts themselves", {

  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))

  test <- node(
    "Y", type="mixture", parents="A",
    distr=list(
      "A==0", node(".", type="gaussian", formula= ~ -2 + B*2, error=1),
      "A==1", node(".", type="gaussian", formula= ~ 3 + B*5, error=1),
      "A==2", node(".", type="conditional_distr", parents=c("A"),
                   distr=distr)
    ))

  out <- str_eq_mixture(test)
  expect_equal(out, c(
   "Y[A==0] ~ N(-2 + B*2, 1)",
   "Y[A==1] ~ N(3 + B*5, 1)",
   "Y[A==2](A=male) ~ N(mean=100, sd=5)",
   "Y[A==2](A=female) ~ Multinomial(probs=c(0Y[A==2]1, 0Y[A==2]2, 0Y[A==2]7))",
   "Y[A==2](other) ~ NA"))
})
