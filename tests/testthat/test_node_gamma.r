
test_that("general test case", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("B", type="gamma", parents="A", betas=0, intercept=log(3),
         shape=4)

  out <- sim_from_dag(dag=dag, n_sim=50000)

  # The conditional mean is constant because the coefficient for A is zero.
  # For a Gamma distribution with mean 3 and shape 4, the variance is 9/4.
  expect_true(all(out$B > 0))
  expect_equal(mean(out$B), 3, tolerance=0.05)
  expect_equal(stats::var(out$B), 9/4, tolerance=0.12)
})

test_that("general test case with enhanced formula", {

  set.seed(2435)

  dag <- empty_dag() +
    node("A", type="rnorm", mean=0, sd=1) +
    node("B", type="gamma", formula= ~ log(3) + A*0, shape=4)

  out <- sim_from_dag(dag=dag, n_sim=50000)

  # The conditional mean is constant because the coefficient for A is zero.
  # For a Gamma distribution with mean 3 and shape 4, the variance is 9/4.
  expect_true(all(out$B > 0))
  expect_equal(mean(out$B), 3, tolerance=0.05)
  expect_equal(stats::var(out$B), 9/4, tolerance=0.12)
})

test_that("calling the function directly with parent variables", {

  data <- data.frame(
    A=c(-1, 0, 1, 2),
    B=c(2, 1, 0, -1)
  )
  mu <- exp(0.5 + data$A*0.2 + data$B*(-0.1))

  set.seed(1234)
  out <- node_gamma(data=data, parents=c("A", "B"),
                    betas=c(0.2, -0.1), intercept=0.5, shape=3)

  set.seed(1234)
  expected <- stats::rgamma(n=nrow(data), shape=3, scale=mu/3)

  expect_equal(out, expected)
  expect_true(all(out > 0))
})

test_that("the function also accepts data.table input", {

  data <- data.table::data.table(A=c(-1, 0, 1))
  mu <- exp(0.3 + data$A*0.2)

  set.seed(1234)
  out <- node_gamma(data=data, parents="A", betas=0.2,
                   intercept=0.3, shape=2)

  set.seed(1234)
  expected <- stats::rgamma(n=nrow(data), shape=2, scale=mu/2)

  expect_equal(out, expected)
})

test_that("the function works with a single observation", {

  data <- data.frame(A=2, B=1)
  mu <- exp(0.5 + 2*0.2 + 1*(-0.1))

  set.seed(1234)
  out <- node_gamma(data=data, parents=c("A", "B"),
                    betas=c(0.2, -0.1), intercept=0.5, shape=3)

  set.seed(1234)
  expected <- stats::rgamma(n=1, shape=3, scale=mu/3)

  expect_length(out, 1)
  expect_equal(out, expected)
})

test_that("using a formula with nonlinear and categorical terms", {

  data <- data.frame(
    A=seq(-1, 1, length.out=100),
    group=factor(rep(c("control", "treated"), each=50),
                 levels=c("control", "treated"))
  )

  # model.matrix() produces columns for A, I(A^2), and grouptreated after
  # removing the intercept column.
  eta <- 0.4 + 0.2*data$A - 0.1*data$A^2 + 0.3*(data$group == "treated")
  mu <- exp(eta)

  set.seed(3456)
  out <- node_gamma(data=data, parents=NULL,
                    formula=~ A + I(A^2) + group,
                    betas=c(0.2, -0.1, 0.3), intercept=0.4, shape=5)

  set.seed(3456)
  expected <- stats::rgamma(n=nrow(data), shape=5, scale=mu/5)

  expect_equal(out, expected)
})

test_that("a formula without an intercept is handled correctly", {

  data <- data.frame(
    A=seq(-1, 1, length.out=100),
    A2=seq(-1, 1, length.out=100)^2
  )

  # The formula design matrix has no intercept column. Its columns should
  # therefore both be retained when calculating the linear predictor.
  set.seed(3456)
  out_formula <- node_gamma(data=data, parents=NULL,
                            formula=~ 0 + A + I(A^2),
                            betas=c(0.2, -0.1), intercept=0.4, shape=5)

  set.seed(3456)
  out_parents <- node_gamma(data=data, parents=c("A", "A2"),
                            betas=c(0.2, -0.1), intercept=0.4, shape=5)

  expect_equal(out_formula, out_parents)
})

test_that("the default link is log", {

  data <- data.frame(A=rep(0, 100))

  set.seed(1234)
  out_default <- node_gamma(data=data, parents="A", betas=0,
                            intercept=log(2), shape=4)

  set.seed(1234)
  out_log <- node_gamma(data=data, parents="A", betas=0,
                        intercept=log(2), shape=4, link="log")

  expect_equal(out_default, out_log)
})

test_that("the log, identity and inverse links are supported", {

  data <- data.frame(A=rep(0, 100))
  link_args <- list(
    log=list(intercept=log(2), link="log"),
    identity=list(intercept=2, link="identity"),
    inverse=list(intercept=0.5, link="inverse")
  )

  for (current_link in link_args) {
    set.seed(7654)
    out <- node_gamma(data=data, parents="A", betas=0,
                      intercept=current_link$intercept, shape=4,
                      link=current_link$link)

    set.seed(7654)
    expected <- stats::rgamma(n=nrow(data), shape=4, scale=rep(2/4, nrow(data)))

    expect_equal(out, expected)
  }
})

test_that("the shape parameter is passed to the Gamma distribution", {

  data <- data.frame(A=rep(0, 100))

  set.seed(8765)
  out <- node_gamma(data=data, parents="A", betas=0,
                    intercept=log(3), shape=2)

  set.seed(8765)
  expected <- stats::rgamma(n=nrow(data), shape=2, scale=rep(3/2, nrow(data)))

  expect_equal(out, expected)
})

test_that("missing predictor values produce missing outcomes", {

  data <- data.frame(A=c(0, NA_real_, 1))

  set.seed(1234)
  out <- suppressWarnings(
    node_gamma(data=data, parents="A", betas=0,
                    intercept=log(2), shape=2)
  )

  expect_true(is.na(out[2]))
  expect_true(all(out[c(1, 3)] > 0))
})

test_that("shape must be a single finite positive numeric value", {

  data <- data.frame(A=rep(0, 3))
  args <- list(data=data, parents="A", betas=0, intercept=1)
  invalid_shapes <- list(0, -1, Inf, -Inf, NA_real_, NaN, c(1, 2), "2", NULL)

  for (invalid_shape in invalid_shapes) {
    expect_error(do.call(node_gamma, c(args, list(shape=invalid_shape))))
  }

  # Integer-valued shape parameters are valid numeric inputs.
  expect_no_error(node_gamma(data=data, parents="A", betas=0,
                             intercept=1, shape=2L))
})

test_that("only the supported link functions are accepted", {

  data <- data.frame(A=rep(0, 3))
  args <- list(data=data, parents="A", betas=0,
               intercept=1, shape=2)
  invalid_links <- list(NA_character_, c("log", "identity"), "probit", "sqrt",
                        1L, NULL)

  for (invalid_link in invalid_links) {
    expect_error(do.call(node_gamma, c(args, list(link=invalid_link))))
  }
})

test_that("the conditional mean must be finite and strictly positive", {

  data <- data.frame(A=rep(0, 3))

  # Invalid means under the identity link.
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=0, shape=2, link="identity"))
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=-1, shape=2, link="identity"))

  # Invalid means under the inverse link.
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=0, shape=2, link="inverse"))
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=-1, shape=2, link="inverse"))

  # Exponentiation can produce zero through underflow or Inf through overflow.
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=-1000, shape=2, link="log"))
  expect_error(node_gamma(data=data, parents="A", betas=0,
                          intercept=1000, shape=2, link="log"))
})

test_that("the structural equation is correct", {

  gamma_node <- node("Y", type="gamma", parents="A", betas=0.5,
                     intercept=1, shape=3)

  expect_equal(
    structural_equation(gamma_node),
    "Y ~ Gamma(shape=3, scale=(exp(1 + 0.5*A))/3)"
  )
})

test_that("the structural equation supports the identity and inverse links", {

  identity_node <- node("Y", type="gamma", parents="A", betas=0.5,
                        intercept=1, shape=3, link="identity")
  inverse_node <- node("Y", type="gamma", parents="A", betas=0.5,
                       intercept=1, shape=3, link="inverse")

  expect_equal(
    structural_equation(identity_node),
    "Y ~ Gamma(shape=3, scale=(1 + 0.5*A)/3)"
  )
  expect_equal(
    structural_equation(inverse_node),
    "Y ~ Gamma(shape=3, scale=(1 / (1 + 0.5*A))/3)"
  )
})

test_that("the structural equation supports formula specifications", {

  gamma_node <- list(
    name="Y",
    type_str="gamma",
    formula="~ -1 + A*0.5",
    link=NULL,
    shape=3
  )

  expect_equal(
    str_eq_gamma(gamma_node),
    "Y ~ Gamma(shape=3, scale=(exp( -1 + A*0.5))/3)"
  )
})

test_that("the structural equation handles nodes without an intercept", {

  gamma_node <- list(
    name="Y",
    type_str="gamma",
    formula=NULL,
    parents="A",
    betas=0.5,
    intercept=NULL,
    shape=3
  )

  expect_equal(str_eq_gamma(gamma_node), "Y ~ Gamma()")
})

test_that("node_gamma can also be supplied as a function type", {

  gamma_node <- node("Y", type=node_gamma, parents="A", betas=0.5,
                     intercept=1, shape=3)

  expect_equal(gamma_node$type_str, "gamma")
  expect_equal(
    structural_equation(gamma_node),
    "Y ~ Gamma(shape=3, scale=(exp(1 + 0.5*A))/3)"
  )
})
