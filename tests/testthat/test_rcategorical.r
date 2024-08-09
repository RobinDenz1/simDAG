
set.seed(53425)

test_that("constant probs, two classes", {

  out <- rcategorical(n=100, probs=c(0.1, 0.9))

  expect_equal(mean(out), 0.93)
})

test_that("constant probs, three classes", {

  out <- rcategorical(n=100, probs=c(0.1, 0.7, 0.2))

  expect_equal(mean(out), 1.07)
})

test_that("matrix probs, two classes", {

  probs <- matrix(c(0.1, 0.9, 0.2, 0.8, 0.3, 0.7, 0.4, 0.6, 0.5, 0.5),
                  ncol=2, byrow=TRUE)
  out <- rcategorical(n=5, probs=probs)

  expect_equal(mean(out), 0.8)
})

test_that("matrix probs, three classes", {

  set.seed(2135)

  probs <- matrix(c(0.1, 0.8, 0.1, 0.2, 0.7, 0.1, 0.3, 0.6, 0.1,
                    0.4, 0.5, 0.1, 0.5, 0.5, 0.0),
                  ncol=3, byrow=TRUE)
  out <- rcategorical(n=5, probs=probs)

  expect_equal(as.vector(table(out)), c(2, 2, 1))
})

test_that("as factor without labels", {

  out <- rcategorical(n=10, probs=c(0.1, 0.9), output="factor")

  expect_equal(levels(out), c("0", "1"))
  expect_true(is.factor(out))
})

test_that("as character without labels", {

  out <- rcategorical(n=10, probs=c(0.1, 0.9), output="character")

  expect_true(all(out %in% c("0", "1")))
  expect_true(!is.factor(out))
})

test_that("setting labels", {

  set.seed(234)
  out <- rcategorical(n=100, probs=c(0.1, 0.9), labels=c("alpha", "beta"),
                      output="character")
  set.seed(234)
  out2 <- rcategorical(n=100, probs=c(0.1, 0.9), labels=c("alpha", "beta"))

  expect_equal(out, out2)
  expect_equal(unique(out), c("beta", "alpha"))
  expect_true(!is.factor(out))
})

test_that("labels work when not all events are observed", {

  set.seed(2345235)

  out <- rcategorical(n=2, probs=c(0.1, 0.2, 0.3, 0.4),
                      labels=c("A", "B", "C", "D"),
                      output="character")

  expect_equal(out, c("A", "D"))
})
