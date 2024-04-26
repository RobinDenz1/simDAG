
test_that("no other", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  test_node <- node("A", type="conditional_distr", parents="sex", distr=distr)
  expect_equal(str_eq_conditional_distr(test_node),
               c("A(sex=male) ~ N(mean=100, sd=5)",
                 "A(sex=female) ~ Multinomial(probs=c(0.1, 0.2, 0.7))",
                 "A(other) ~ NA"))
})

test_that("with default distr", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  test_node <- node("A", type="conditional_distr", parents="sex", distr=distr,
                    default_distr="rnorm", default_distr_args=list(mean=20))
  expect_equal(str_eq_conditional_distr(test_node),
               c("A(sex=male) ~ N(mean=100, sd=5)",
                 "A(sex=female) ~ Multinomial(probs=c(0.1, 0.2, 0.7))",
                 "A(other) ~ N(20)"))
})

test_that("with default value", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  test_node <- node("A", type="conditional_distr", parents="sex", distr=distr,
                    default_val=21)
  expect_equal(str_eq_conditional_distr(test_node),
               c("A(sex=male) ~ N(mean=100, sd=5)",
                 "A(sex=female) ~ Multinomial(probs=c(0.1, 0.2, 0.7))",
                 "A(other) ~ 21"))
})
