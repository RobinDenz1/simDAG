
test_that("general test case", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("B", type="beta", parents="A", betas=0,
         intercept=qlogis(0.4), precision=5)

  out <- sim_from_dag(dag=dag, n_sim=50000)

  # For a Beta distribution with mean 0.4 and precision 5, the variance is
  # 0.4 * 0.6 / (5 + 1) = 0.04.
  expect_true(all(out$B > 0 & out$B < 1))
  expect_equal(mean(out$B), 0.4, tolerance=0.01)
  expect_equal(stats::var(out$B), 0.04, tolerance=0.002)
})

test_that("general test case, using enhanced formula", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("B", type="beta", formula= ~ qlogis(0.4) + A*0, precision=5)

  out <- sim_from_dag(dag=dag, n_sim=50000)

  # For a Beta distribution with mean 0.4 and precision 5, the variance is
  # 0.4 * 0.6 / (5 + 1) = 0.04.
  expect_true(all(out$B > 0 & out$B < 1))
  expect_equal(mean(out$B), 0.4, tolerance=0.01)
  expect_equal(stats::var(out$B), 0.04, tolerance=0.002)
})

test_that("calling the function directly with parent variables", {

  data <- data.frame(
    A=c(-1, 0, 1, 2),
    B=c(2, 1, 0, -1)
  )
  mu <- stats::plogis(0.5 + data$A*0.2 + data$B*(-0.1))

  set.seed(1234)
  out <- node_beta(data=data, parents=c("A", "B"),
                   betas=c(0.2, -0.1), intercept=0.5, precision=7)

  set.seed(1234)
  expected <- stats::rbeta(
    n=nrow(data), shape1=mu*7, shape2=(1-mu)*7
  )

  expect_equal(out, expected)
  expect_true(all(out > 0 & out < 1))
})

test_that("the function also accepts data.table input", {

  data <- data.table::data.table(A=c(-1, 0, 1))
  mu <- stats::plogis(0.3 + data$A*0.2)

  set.seed(1234)
  out <- node_beta(data=data, parents="A", betas=0.2,
                   intercept=0.3, precision=4)

  set.seed(1234)
  expected <- stats::rbeta(
    n=nrow(data), shape1=mu*4, shape2=(1-mu)*4
  )

  expect_equal(out, expected)
})

test_that("the function works with a single observation", {

  data <- data.frame(A=2, B=1)
  mu <- stats::plogis(0.5 + 2*0.2 + 1*(-0.1))

  set.seed(1234)
  out <- node_beta(data=data, parents=c("A", "B"),
                   betas=c(0.2, -0.1), intercept=0.5, precision=3)

  set.seed(1234)
  expected <- stats::rbeta(n=1, shape1=mu*3, shape2=(1-mu)*3)

  expect_length(out, 1)
  expect_equal(out, expected)
})

test_that("using a formula with nonlinear and categorical terms", {

  data <- data.frame(
    A=seq(-1, 1, length.out=100),
    group=factor(rep(c("control", "treated"), each=50),
                 levels=c("control", "treated"))
  )

  eta <- 0.4 + 0.2*data$A - 0.1*data$A^2 + 0.3*(data$group == "treated")
  mu <- stats::plogis(eta)

  set.seed(3456)
  out <- node_beta(data=data, parents=NULL,
                   formula=~ A + I(A^2) + group,
                   betas=c(0.2, -0.1, 0.3), intercept=0.4,
                   precision=5)

  set.seed(3456)
  expected <- stats::rbeta(
    n=nrow(data), shape1=mu*5, shape2=(1-mu)*5
  )

  expect_equal(out, expected)
})

test_that("a formula without an intercept is handled correctly", {

  data <- data.frame(
    A=seq(-1, 1, length.out=100),
    A2=seq(-1, 1, length.out=100)^2
  )

  set.seed(3456)
  out_formula <- node_beta(data=data, parents=NULL,
                           formula=~ 0 + A + A2,
                           betas=c(0.2, -0.1), intercept=0.4,
                           precision=5)

  set.seed(3456)
  out_parents <- node_beta(data=data, parents=c("A", "A2"),
                           betas=c(0.2, -0.1), intercept=0.4,
                           precision=5)

  expect_equal(out_formula, out_parents)
})

test_that("a formula containing only an intercept is handled correctly", {

  data <- data.frame(A=seq_len(100))
  mu <- stats::plogis(0.3)

  set.seed(1234)
  out <- node_beta(data=data, parents=NULL, formula=~ 1,
                   betas=numeric(0), intercept=0.3, precision=6)

  set.seed(1234)
  expected <- stats::rbeta(
    n=nrow(data), shape1=mu*6, shape2=(1-mu)*6
  )

  expect_equal(out, expected)
})

test_that("the default link is logit", {

  data <- data.frame(A=rep(0, 100))

  set.seed(1234)
  out_default <- node_beta(data=data, parents="A", betas=0,
                           intercept=0.2, precision=5)

  set.seed(1234)
  out_logit <- node_beta(data=data, parents="A", betas=0,
                         intercept=0.2, precision=5, link="logit")

  expect_equal(out_default, out_logit)
})

test_that("all supported link functions are implemented correctly", {

  data <- data.frame(A=rep(0, 100))
  eta <- 0.25
  precision <- 6

  expected_mu <- list(
    logit=stats::plogis(eta),
    probit=stats::pnorm(eta),
    cloglog=-expm1(-exp(eta)),
    loglog=exp(-exp(-eta)),
    cauchit=stats::pcauchy(eta)
  )

  for (current_link in names(expected_mu)) {
    set.seed(7654)
    out <- node_beta(data=data, parents="A", betas=0,
                     intercept=eta, precision=precision,
                     link=current_link)

    set.seed(7654)
    expected <- stats::rbeta(
      n=nrow(data),
      shape1=expected_mu[[current_link]]*precision,
      shape2=(1-expected_mu[[current_link]])*precision
    )

    expect_equal(out, expected)
  }
})

test_that("precision determines the beta distribution shape parameters", {

  data <- data.frame(A=rep(0, 100))
  mu <- 0.4
  precision <- 2.5

  set.seed(8765)
  out <- node_beta(data=data, parents="A", betas=0,
                   intercept=qlogis(mu), precision=precision)

  set.seed(8765)
  expected <- stats::rbeta(
    n=nrow(data), shape1=mu*precision,
    shape2=(1-mu)*precision
  )

  expect_equal(out, expected)
})

test_that("missing predictor values produce missing outcomes", {

  data <- data.frame(A=c(0, NA_real_, 1))

  set.seed(1234)
  out <- suppressWarnings(
    node_beta(data=data, parents="A", betas=0, intercept=0.2, precision=5)
  )

  expect_true(is.na(out[2]))
  expect_true(all(out[c(1, 3)] > 0 & out[c(1, 3)] < 1))
})

test_that("precision must be a single finite positive numeric value", {

  data <- data.frame(A=rep(0, 3))
  args <- list(data=data, parents="A", betas=0, intercept=0.2)
  invalid_precisions <- list(0, -1, Inf, -Inf, NA_real_, NaN, c(1, 2),
                             "2", NULL)

  for (invalid_precision in invalid_precisions) {
    expect_error(do.call(
      node_beta, c(args, list(precision=invalid_precision))
    ))
  }

  # Integer-valued precision parameters are valid numeric inputs.
  expect_length(node_beta(data=data, parents="A", betas=0,
                          intercept=0.2, precision=2L), 3)
})

test_that("only the supported link functions are accepted", {

  data <- data.frame(A=rep(0, 3))
  args <- list(data=data, parents="A", betas=0,
               intercept=0.2, precision=5)
  invalid_links <- list(NA_character_, c("logit", "probit"), "identity",
                        "log", "sqrt", 1L, NULL)

  for (invalid_link in invalid_links) {
    expect_error(do.call(
      node_beta, c(args, list(link=invalid_link))
    ))
  }
})

test_that("the conditional mean must be finite and strictly between zero and one", {

  data <- data.frame(A=rep(0, 3))

  # Extreme linear predictors can produce numerical values at the boundaries.
  expect_error(node_beta(data=data, parents="A", betas=0,
                         intercept=-1000, precision=5, link="logit"))
  expect_error(node_beta(data=data, parents="A", betas=0,
                         intercept=1000, precision=5, link="logit"))

  # NaN values are invalid, whereas ordinary missing values are propagated.
  data_nan <- data.frame(A=NaN)
  expect_error(node_beta(data=data_nan, parents="A", betas=1,
                         intercept=0.2, precision=5))
})

test_that("beta structural equations are correctly generated", {

  nd <- node("Y", type="beta",
             parents=c("X1", "X2"),
             betas=c(0.6, -0.8),
             intercept=-0.3,
             precision=20)

  eq <- structural_equation(nd)

  expect_true(grepl("Y ~ Beta(shape1=", eq, fixed=TRUE))
  expect_true(grepl("shape2=", eq, fixed=TRUE))
  expect_true(grepl("logit(-0.3 + 0.6*X1 + -0.8*X2)",
                    eq, fixed=TRUE))
  expect_true(grepl("*20", eq, fixed=TRUE))
})

test_that("beta structural equations support formula specifications", {

  nd <- node("Y", type="beta",
             formula= ~ 0.2 + X*0.4,
             precision=12)

  eq <- structural_equation(nd)

  expect_true(grepl("Y ~ Beta(shape1=", eq, fixed=TRUE))
  expect_true(grepl("logit(", eq, fixed=TRUE))
  expect_true(grepl("0.2 + X*0.4", eq, fixed=TRUE))
  expect_true(grepl("*12", eq, fixed=TRUE))
})

test_that("beta structural equations support all link functions", {

  link_prefix <- c(
    logit="logit(",
    probit="pnorm(",
    cloglog="1 - exp(-exp(",
    loglog="exp(-exp(-(",
    cauchit="pcauchy("
  )

  for (current_link in names(link_prefix)) {

    nd <- node("Y", type="beta",
               parents="X",
               betas=0.5,
               intercept=0.2,
               precision=10,
               link=current_link)

    eq <- structural_equation(nd)

    expect_true(
      grepl(link_prefix[[current_link]], eq, fixed=TRUE),
      info=paste("Failed for link:", current_link)
    )
  }
})

test_that("beta structural equations are correctly printed in DAG summaries", {

  dag <- empty_dag() +
    node("X", type="rnorm", mean=0, sd=1) +
    node("Y", type="beta",
         parents="X",
         betas=0.5,
         intercept=-0.3,
         precision=15)

  printed <- paste(capture.output(summary(dag)), collapse="\n")

  expect_true(grepl("Y ~ Beta(shape1=", printed, fixed=TRUE))
  expect_true(grepl("shape2=", printed, fixed=TRUE))
  expect_true(grepl("logit(-0.3 + 0.5*X)", printed, fixed=TRUE))
  expect_true(grepl("*15", printed, fixed=TRUE))
})
