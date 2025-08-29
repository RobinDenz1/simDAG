
test_that("general test case", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", parents=c("A", "B"), betas=c(10, 11),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 101.96705, tolerance=0.001)
})

test_that("calling the function directly", {

  set.seed(43525)

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1)
  data <- as.data.frame(sim_from_dag(dag, n_sim=1000))

  out <- node_gaussian(data=data, parents=c("A", "B"), betas=c(10, 11),
                       intercept=-100, error=10)

  expect_equal(mean(out), 101.96705, tolerance=0.001)
})

## classic formula

test_that("with formula", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", formula=~ A + B, betas=c(10, 11),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 101.96705, tolerance=0.001)
})

test_that("with formula + quadratic effects", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=0, sd=1) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ A + I(A^2) + B,
         betas=c(0.2, 0.1, 0.5),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -99.67325, tolerance=0.001)
})

test_that("with formula + categorical independent variable", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rcategorical", probs=c(0.1, 0.3, 0.6), output="factor") +
    node("gauss", type="gaussian", formula=~ A + B, betas=c(10, 11, -10),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 99.10283, tolerance=0.001)
})

test_that("with formula + interaction term", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ A + B + A*B, betas=c(10, 11, -20),
         intercept=-100, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -83.31415, tolerance=0.001)
})

test_that("using different link functions", {

  set.seed(43525)

  ## link = "log"
  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", formula=~ -1 + A*-2 + BTRUE*11, error=1,
         link="log")
  dat <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(dat$gauss), 3), -0.001)

  ## link = "inverse"
  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", formula=~ -1 + A*-2 + BTRUE*11, error=1,
         link="inverse")
  dat <- sim_from_dag(dag, n_sim=1000)
  expect_equal(round(mean(dat$gauss), 3), -0.05)
})

test_that("input checks with link argument", {

  # wrong input
  expect_error({
    dag <- empty_dag() +
      node("A", type="gaussian", formula=~ -1 + A*-2 + BTRUE*11, error=1,
           link=c("something", "22"))
  })

  # unsupported link
  expect_error({
    dag <- empty_dag() +
      node("A", type="gaussian", formula=~ -1 + A*-2 + BTRUE*11, error=1,
           link="logit")
  })
})

## special formula interface

test_that("with special formula", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.1) +
    node("gauss", type="gaussian", formula=~ -100 + A*10 + BTRUE*11, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 101.96705, tolerance=0.001)
})

test_that("with special formula + quadratic effects", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=0, sd=1) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ -100 + A*0.2 + I(A^2)*0.1 +
           BTRUE*0.5, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -99.67325, tolerance=0.001)
})

test_that("with special formula + categorical independent variable", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rcategorical", probs=c(0.1, 0.3, 0.6), output="factor") +
    node("gauss", type="gaussian", formula=~ -100 + A*10 + B1*11 + B2*-10,
         error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 99.10283, tolerance=0.001)
})

test_that("with special formula + interaction term", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ -100 + A*10 + BTRUE*11 +
           A:BTRUE*-20, error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -83.31415, tolerance=0.001)
})

test_that("with special formula in reverse order", {
  dag <- empty_dag() +
    node("A", type="rnorm") +
    node("B", type="rbernoulli") +
    node("testin", type="rcategorical", probs=c(0.1, 0.2, 0.2, 0.5),
         output="factor") +
    node("C", type="gaussian", parents=c("A", "B"), betas=c(0.1, 2),
         error=0.001, intercept=-2)

  dag1 <- dag +
    node("D", type="binomial", formula=~ -2 + 0.1*A + 2*BTRUE +
           0.3*A:C + 2*I(A^2))

  dag2 <- dag +
    node("D", type="binomial", formula=~ -2 + A*0.1 + BTRUE*2 +
           A:C*0.3 + I(A^2)*2)

  set.seed(2345)
  data1 <- sim_from_dag(dag1, n_sim=100, sort_dag=FALSE)

  set.seed(2345)
  data2 <- sim_from_dag(dag2, n_sim=100, sort_dag=FALSE)

  expect_equal(data1, data2)
})

test_that("with special formula + only function calls on numbers", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ -100 + A*log(10) + BTRUE*log(11),
         error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -52.60826, tolerance=0.001)
})

test_that("with special formula + external coefficients", {

  some_var <- 10
  some_var2 <- 11

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B", "rbernoulli", p=0.5) +
    node("gauss", type="gaussian", formula=~ -100 + A*eval(some_var) +
           BTRUE*eval(some_var2), error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), 106.147, tolerance=0.001)
})

test_that("with special formula + special characters in it", {

  dag <- empty_dag() +
    node("A", "rnorm", mean=20, sd=5) +
    node("B-/", "rbernoulli", p=0.5, output="numeric") +
    node("gauss", type="gaussian", formula=~ -100 + A*log(10) + `B-/`*log(11),
         error=10)

  set.seed(43525)

  dat <- sim_from_dag(dag, n_sim=1000)

  expect_equal(mean(dat$gauss), -52.60826, tolerance=0.001)
})

test_that("with special formula but NA in betas", {
  expect_error({
    dag <- empty_dag() +
      node("A", type="rnorm") +
      node("C", type="rbernoulli") +
      node("B", type="gaussian", formula= ~ -2 + A*NA + C*2)
  },
  "One or more of the supplied beta coefficients in 'formula' are not numbers.")
})
