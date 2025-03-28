
test_that("general test case", {

  node <- node("Bla", type="aftreg", formula= ~ -2 + A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2)

  out1 <- str_eq_rsurv(node, "aftreg")
  out2 <- str_eq_aftreg(node)

  expect_equal(out1, out2)
  expect_equal(out1, c("Bla[T] ~ aftreg(-2 + A*0.1 + B*3, dist='weibull')",
                       "Bla[C] ~ Inf"))
})

test_that("with censoring", {

  node <- node("Bla2", type="ahreg", formula= ~ -2 + A*0.1 + B*3,
               baseline="weibull", shape=1, scale=2,
               cens_dist="runif", cens_args=list(min=0, max=10))

  out <- str_eq_rsurv(node, "aftreg")
  expect_equal(out, c("Bla2[T] ~ aftreg(-2 + A*0.1 + B*3, dist='weibull')",
                      "Bla2[C] ~ runif(min=0, max=10)"))
})
