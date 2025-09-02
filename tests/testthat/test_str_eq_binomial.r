
test_that("unspecified node", {
  test_node <- node("A", type="binomial", parents=c("C", "D"))
  expect_equal(str_eq_binomial(test_node), "A ~ Bernoulli()")
})

test_that("one parent", {
  test_node <- node("A", type="binomial", parents="C", betas=1.2,
                    intercept=-12)
  expect_equal(str_eq_binomial(test_node), "A ~ Bernoulli(logit(-12 + 1.2*C))")

  # link="identity"
  test_node$link <- "identity"
  expect_equal(str_eq_binomial(test_node), "A ~ Bernoulli(-12 + 1.2*C)")

  # link="probit"
  test_node$link <- "probit"
  expect_equal(str_eq_binomial(test_node), "A ~ Bernoulli(pnorm(-12 + 1.2*C))")

  # link="log"
  test_node$link <- "log"
  expect_equal(str_eq_binomial(test_node), "A ~ Bernoulli(exp(-12 + 1.2*C))")

  # link="cloglog"
  test_node$link <- "cloglog"
  expect_equal(str_eq_binomial(test_node),
               "A ~ Bernoulli(1 - exp(-exp(-12 + 1.2*C)))")

  # link="cauchit"
  test_node$link <- "cauchit"
  expect_equal(str_eq_binomial(test_node),
               "A ~ Bernoulli(pcauchy(-12 + 1.2*C))")
})

test_that("one parent with return_prob", {
  test_node <- node("A", type="binomial", parents="C", betas=1.2,
                    intercept=-12, return_prob=TRUE)
  expect_equal(str_eq_binomial(test_node), "A ~ logit(-12 + 1.2*C)")
})

test_that("multiple parents", {
  test_node <- node("A", type="binomial", parents=c("C", "D", "E"),
                    betas=c(1.2, 0.3, 7), intercept=-12)
  expect_equal(str_eq_binomial(test_node),
               "A ~ Bernoulli(logit(-12 + 1.2*C + 0.3*D + 7*E))")
})
