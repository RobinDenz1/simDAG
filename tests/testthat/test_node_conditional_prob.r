
test_that("two classes, one parent node", {

  set.seed(42)

  probs <- list(male=0.5, female=0.8)

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents="sex", probs=probs)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_true(sum(data$A)==656)
})

test_that("two classes, one parent node, labels & factor", {

  set.seed(42)

  probs <- list(male=0.5, female=0.8)

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents="sex", probs=probs,
         labels=c("A", "B"), coerce2factor=TRUE)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_true(sum(data$A=="B")==656)
})

test_that("two classes, one parent node, factor", {

  set.seed(42)

  probs <- list(male=0.5, female=0.8)

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents="sex", probs=probs,
         coerce2factor=TRUE)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_true(sum(data$A=="TRUE")==656)
})

test_that("two classes, one parent node with default_prob", {

  set.seed(42)

  probs <- list(male=0.5)

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents="sex", probs=probs,
         default_prob=0.8)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_true(sum(data$A)==656)
})

test_that("three classes, one parent node", {

  set.seed(42)

  probs <- list(male=c(0.5, 0.2, 0.3), female=c(0.8, 0.1, 0.1))

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents="sex", probs=probs)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_equal(as.vector(table(data$A)), c(641, 161, 198))
})

test_that("two classes, two parent nodes", {

  set.seed(3546)

  probs <- list(male.FALSE=0.5,
                male.TRUE=0.8,
                female.FALSE=0.1,
                female.TRUE=0.3)

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents=c("sex", "chemo"), probs=probs)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_equal(as.vector(table(data$A)), c(575, 425))
})

test_that("three classes, two parent nodes", {

  set.seed(452345)

  probs <- list(male.FALSE=c(0.5, 0.1, 0.4),
                male.TRUE=c(0.8, 0.1, 0.1),
                female.FALSE=c(0.1, 0.7, 0.2),
                female.TRUE=c(0.3, 0.4, 0.3))

  dag <- empty_dag() +
    node("sex", type="rcategorical", labels=c("male", "female"),
         coerce2factor=TRUE, probs=c(0.5, 0.5)) +
    node("chemo", type="rbernoulli", p=0.5) +
    node("A", type="conditional_prob", parents=c("sex", "chemo"), probs=probs)

  data <- sim_from_dag(dag=dag, n_sim=1000)

  expect_equal(as.vector(table(data$A)), c(412, 318, 270))
})
