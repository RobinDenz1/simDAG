
data <- data.table(sex=c("female", "female", "male"),
                   uicc=c("II", "I", "II"))

test_that("unequal number of probabilities", {
  probs <- list(female=c(0.1, 0.9), male=c(0.1))
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs))
})

test_that("level not in data", {
  probs <- list(nofemale=0.9, male=c(0.1))
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs))
})

test_that("list without names", {
  probs <- list(c(0.1, 0.9), c(0.1, 0.3))
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs))
})

test_that("level not in data: interaction", {
  probs <- list(nofemale=0.9, male=0.1)
  expect_error(node_conditional_prob(data=data, parents=c("sex", "uicc"),
                                     probs=probs))
})

test_that("wrong default_probs", {
  probs <- list(female=0.9, male=0.1)
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs,
                                     default_probs=10))
})

test_that("wrong length default_probs", {
  probs <- list(female=0.9, male=0.1)
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs,
                                     default_probs=c(0.1, 0.8)))
})

test_that("wrong default_val", {
  probs <- list(female=0.9, male=0.1)
  expect_error(node_conditional_prob(data=data, parents="sex", probs=probs,
                                     default_val=c(1, 2)))
})
