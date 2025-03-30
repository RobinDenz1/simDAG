
test_that("general test case", {

  node <- node("Bla", type="aftreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)

  out1 <- str_eq_rsurv(node, "aftreg")
  out2 <- str_eq_aftreg(node)

  expect_equal(out1, out2)
  expect_equal(out1, c("Bla[T] ~ aftreg(A*0.1 + B*3, dist='weibull')",
                       "Bla[C] ~ Inf"))
})

test_that("with censoring", {

  node <- node("Bla2", type="ahreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2,
               cens_dist="runif", cens_args=list(min=0, max=10))

  out <- str_eq_rsurv(node, "aftreg")
  expect_equal(out, c("Bla2[T] ~ aftreg(A*0.1 + B*3, dist='weibull')",
                      "Bla2[C] ~ runif(min=0, max=10)"))
})

test_that("with parents / betas specified", {
  node <- node("Bla", type="aftreg",
               parents=c("A", "B"),
               betas=c(0.1, 3),
               dist="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ aftreg(0.1*A + 3*B, dist='weibull')",
                      "Bla[C] ~ Inf"))
})

test_that("correct output with structural_equation()", {

  # aftreg
  node <- node("Bla", type="aftreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ aftreg(A*0.1 + B*3, dist='weibull')",
                      "Bla[C] ~ Inf"))

  # ahreg
  node <- node("Bla", type="ahreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ ahreg(A*0.1 + B*3, dist='weibull')",
                      "Bla[C] ~ Inf"))

  # ehreg
  node <- node("Bla", type="ehreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ ehreg(A*0.1 + B*3, dist='weibull')",
                      "Bla[C] ~ Inf"))

  # poreg
  node <- node("Bla", type="poreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ poreg(A*0.1 + B*3, dist='weibull')",
                      "Bla[C] ~ Inf"))

  # ypreg
  node <- node("Bla", type="ypreg", formula= ~ A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)
  out <- structural_equation(node)
  expect_equal(out, c("Bla[T] ~ ypreg(A*0.1 + B*3, dist='weibull')",
                      "Bla[C] ~ Inf"))
})
