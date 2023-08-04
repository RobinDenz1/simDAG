
data <- data.table(sex=c("female", "female", "male"),
                   uicc=c("II", "I", "II"))

test_that("no names in list", {
  distr <- list("s", "a")
  expect_error(node_conditional_distr(data=data, parents="sex", distr=distr))
})

test_that("wrong level in distr", {
  distr <- list(nomale=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  expect_error(node_conditional_distr(data=data, parents="sex", distr=distr))
})

test_that("wrong level in distr: interaction", {
  distr <- list(nomale=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  expect_error(node_conditional_distr(data=data, parents=c("sex", "uicc"),
                                      distr=distr))
})

test_that("wrong default_distr", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  expect_error(node_conditional_distr(data=data, parents="sex", distr=distr,
                                      default_distr="lol"))
})

test_that("wrong default_val", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  expect_error(node_conditional_distr(data=data, parents="sex", distr=distr,
                                      default_val=c(1, 2)))
})

test_that("wrong coerce2numeric", {
  distr <- list(male=list("rnorm", mean=100, sd=5),
                female=list("rcategorical", probs=c(0.1, 0.2, 0.7)))
  expect_error(node_conditional_distr(data=data, parents="sex", distr=distr,
                                      coerce2numeric="ax"))
})
