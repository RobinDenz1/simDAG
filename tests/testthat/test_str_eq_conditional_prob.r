
test_that("two classes, no other", {
  probs <- list(male=0.5, female=0.8)
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs)
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Bernoulli(0.5)",
                 "A(sex=female) ~ Bernoulli(0.8)",
                 "A(other) ~ NA"))
})

test_that("two classes, with default prob", {
  probs <- list(male=0.5)
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs,
                    default_probs=0.2)
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Bernoulli(0.5)", "A(other) ~ Bernoulli(0.2)"))
})

test_that("two classes, with default value", {
  probs <- list(male=0.5)
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs,
                    default_val=0)
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Bernoulli(0.5)", "A(other) ~ 0"))
})

test_that("three classes, no other", {
  probs <- list(male=c(0.5, 0.2, 0.3), female=c(0.8, 0.1, 0.1))
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs)
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Multinomial(0.5, 0.2, 0.3)",
                 "A(sex=female) ~ Multinomial(0.8, 0.1, 0.1)",
                 "A(other) ~ NA"))
})

test_that("three classes, with default prob", {
  probs <- list(male=c(0.5, 0.2, 0.3))
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs,
                    default_probs=c(0.2, 0.4, 0.4))
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Multinomial(0.5, 0.2, 0.3)",
                 "A(other) ~ Multinomial(0.2, 0.4, 0.4)"))
})

test_that("three classes, with default value", {
  probs <- list(male=c(0.5, 0.2, 0.3))
  test_node <- node("A", type="conditional_prob", parents="sex", probs=probs,
                    default_val=2)
  expect_equal(str_eq_conditional_prob(test_node),
               c("A(sex=male) ~ Multinomial(0.5, 0.2, 0.3)",
                 "A(other) ~ 2"))
})
