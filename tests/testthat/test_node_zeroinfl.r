
test_that("zero-inflated poisson regression", {

  # with parents specified
  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
         formula_zero= ~ 1 + A*1 + B*2,
         family_count="poisson",
         parents=c("A", "B"))
  data1 <- sim_from_dag(dag, n_sim=100)

  # without parents specified
  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
         formula_zero= ~ 1 + A*1 + B*2,
         family_count="poisson")
  data2 <- sim_from_dag(dag, n_sim=100)

  expect_equal(data1, data2)
})

test_that("zero-inflated poisson regression, count formula zero other", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
         parents_zero=c("A", "B"),
         betas_zero=c(1, 2),
         intercept_zero=1,
         family_count="poisson")
  data <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data$Y), 3), 0.02)
})

test_that("zero-inflated poisson regression, zero formula count other", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         parents_count=c("A", "B"),
         betas_count=c(0.2, 0.1),
         intercept_count=-2,
         formula_zero= ~ 1 + A*1 + B*2,
         family_count="poisson")
  data <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data$Y), 3), 0.02)
})

test_that("all specified with individual components", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         parents_count=c("A", "B"),
         betas_count=c(0.2, 0.1),
         intercept_count=-2,
         parents_zero=c("A", "B"),
         betas_zero=c(1, 2),
         intercept_zero=1,
         family_count="poisson")
  data <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data$Y), 3), 0.02)
})

test_that("zero-inflated negative-binomial regression", {

  set.seed(134)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*3 + A:B*0.4,
         formula_zero= ~ 3 + A*0.1 + B*0.3,
         family_count="negative_binomial", theta=1,
         parents=c("A", "B"))
  data <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data$Y), 3), 7.17)
})

test_that("with random effects", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("C", type="rcategorical", probs=rep(0.1, 10),
         labels=LETTERS[1:10]) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4 + (1|C),
         formula_zero= ~ 1 + A*1 + B*2 + (1|C),
         family_count="poisson",
         parents=c("A", "B"),
         var_corr_count=0.5,
         var_corr_zero=0.1)
  data <- sim_from_dag(dag, n_sim=100)
  expect_equal(round(mean(data$Y), 3), 0.13)
})

test_that("works in sim_discrete_time() as well", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node_td("Y", type="zeroinfl",
            parents_count=c("A", "B"),
            betas_count=c(0.2, 0.1),
            intercept_count=-2,
            formula_zero= ~ 1 + A*1 + B*2,
            family_count="poisson")
  data <- sim_discrete_time(dag, n_sim=100, max_t=5)

  expect_equal(round(mean(data$data$Y), 3), 0.13)
})

test_that("works with links", {

  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
         formula_zero= ~ 1 + A*1 + B*2,
         family_count="poisson",
         link_count="sqrt",
         link_zero="log")
  data1 <- sim_from_dag(dag, n_sim=100)

  # with defaults
  set.seed(234)

  dag <- empty_dag() +
    node(c("A", "B"), type="rnorm", mean=0, sd=1) +
    node("Y", type="zeroinfl",
         formula_count= ~ -2 + A*0.2 + B*0.1 + A:B*0.4,
         formula_zero= ~ 1 + A*1 + B*2,
         family_count="poisson",
         link_count="log",
         link_zero="logit")
  data2 <- sim_from_dag(dag, n_sim=100)

  expect_equal(round(mean(data1$Y), 3), 2.88)
  expect_equal(round(mean(data2$Y, 3)), 0)
})

test_that("missing info on parents in count model", {

  expect_error({
    dag <- empty_dag() +
      node(c("A", "B"), type="rnorm", mean=0, sd=1) +
      node("Y", type="zeroinfl",
           formula_zero= ~ 3 + A*0.1 + B*0.3,
           family_count="negative_binomial", theta=1)
  }, paste0("Either 'parents_count' or 'formula_count' must be specified ",
            "when using type='zeroinfl'."))
})

test_that("missing info on parents in zero model", {

  expect_error({
    dag <- empty_dag() +
      node(c("A", "B"), type="rnorm", mean=0, sd=1) +
      node("Y", type="zeroinfl",
           formula_count= ~ 3 + A*0.1 + B*0.3,
           family_count="negative_binomial", theta=1)
  }, paste0("Either 'parents_zero' or 'formula_zero' must be specified ",
            "when using type='zeroinfl'."))
})
