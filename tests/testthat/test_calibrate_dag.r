
.make_normal_ldag <- function() {
  lazy_dag(
    empty_dag() +
      node("X", type="rnorm", mean=param(mu), sd=1)
  )
}

.make_two_normal_ldag <- function() {
  lazy_dag(
    empty_dag() +
      node("X", type="rnorm", mean=param(mu_X), sd=1) +
      node("Z", type="rnorm", mean=param(mu_Z), sd=1)
  )
}

.mean_target <- function(variable, value, scale=NULL) {
  out <- list(
    target = value,
    eval_fun = function(data) mean(data[[variable]])
  )

  if (!is.null(scale)) {
    out$scale <- scale
  }

  return(out)
}

.small_optim_control <- function(...) {
  modifyList(list(maxit=200, trace=0), list(...))
}

# ------------------------------------------------------------
# Parameter specification and root finding
# ------------------------------------------------------------

test_that("a single bounded parameter uses uniroot()", {

  ldag <- .make_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value=1.25)
  )

  # flat, non-nested specification for one parameter
  params <- list(
    start = 0,
    range = c(-5, 5)
  )

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    n_sim_run = 300,
    n_sim_eval = 300,
    seed = 123,
    optim_control = .small_optim_control()
  )

  expect_s3_class(fit, "calibrated_DAG")
  expect_null(fit$optim_obj)
  expect_true(!is.null(fit$uniroot_obj))

  expect_named(fit$est_params, "mu")
  expect_equal(unname(fit$target_eval), 1.25, tolerance=1e-3)
  expect_true(!is.null(fit$dag))
})

test_that("a single parameter without bounds uses optim()", {

  ldag <- .make_normal_ldag()

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = list(
      mean_X = .mean_target("X", value=0.75)
    ),
    params = list(start = 0),
    n_sim_run = 300,
    n_sim_eval = 300,
    seed = 123,
    optim_control = .small_optim_control()
  )

  expect_null(fit$uniroot_obj)
  expect_true(!is.null(fit$optim_obj))
  expect_equal(fit$inputs$optim_method, "L-BFGS-B")
  expect_named(fit$est_params, "mu")
  expect_equal(unname(fit$target_eval), 0.75, tolerance=1e-3)
})


# ------------------------------------------------------------
# Multiple parameters and targets
# ------------------------------------------------------------

test_that("multiple parameters and targets can be calibrated", {

  ldag <- .make_two_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 1.25, scale = 0.1),
    mean_Z = .mean_target("Z", value = -0.75, scale = 0.2)
  )

  # test both named numeric vectors and nested lists.
  params <- list(
    mu_X = c(start = 0),
    mu_Z = list(start = 0, scale = 2)
  )

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    n_sim_run = 500,
    n_sim_eval = 500,
    seed = 321,
    optim_control = .small_optim_control(maxit = 500)
  )

  expect_s3_class(fit, "calibrated_DAG")
  expect_null(fit$uniroot_obj)
  expect_true(!is.null(fit$optim_obj))

  expect_named(fit$est_params, c("mu_X", "mu_Z"))

  expect_equal(
    unname(fit$target_eval),
    c(1.25, -0.75),
    tolerance = 1e-3
  )

  # Default parameter scaling:
  # mu_X: max(1, abs(start)) = 1
  # mu_Z: explicitly supplied scale = 2
  expect_equal(unname(fit$inputs$optim_control$parscale), c(1, 2))
})

test_that("finite parameter ranges determine default parscale", {

  ldag <- .make_two_normal_ldag()

  params <- list(
    mu_X = list(start = 0, range = c(-4, 4)),
    mu_Z = list(start = 0, range = c(-6, 6))
  )

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = list(
      mean_X = .mean_target("X", value = 0.2),
      mean_Z = .mean_target("Z", value = -0.2)
    ),
    params = params,
    n_sim_run = 100,
    n_sim_eval = 100,
    seed = 42,
    optim_control = .small_optim_control(maxit = 1)
  )

  expect_equal(unname(fit$inputs$optim_control$parscale), c(4, 6))
})

test_that("user-supplied parscale takes precedence", {

  fit <- calibrate_dag(
    lazy_dag = .make_two_normal_ldag(),
    targets = list(
      mean_X = .mean_target("X", value = 0.5),
      mean_Z = .mean_target("Z", value = -0.5)
    ),
    params = list(
      mu_X = list(start = 0),
      mu_Z = list(start = 0)
    ),
    n_sim_run = 100,
    n_sim_eval = 100,
    seed = 123,
    optim_control = .small_optim_control(
      maxit = 1,
      parscale = c(mu_X = 3, mu_Z = 4)
    )
  )

  expect_equal(unname(fit$inputs$optim_control$parscale), c(3, 4))
})

# ------------------------------------------------------------
# Target scaling and custom objective functions
# ------------------------------------------------------------

test_that("target scales are used in the default objective", {

  ldag <- .make_two_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 1, scale = 0.25),
    mean_Z = .mean_target("Z", value = -1)
  )

  params <- c(mu_X = 0.2, mu_Z = -0.3)

  achieved <- estimate_targets(
    lazy_dag = ldag,
    params = params,
    targets = targets,
    n_sim = 200,
    seed = 123
  )

  residuals <- (achieved - c(mean_X = 1, mean_Z = -1)) /
    c(mean_X = 0.25, mean_Z = 1)

  obj <- get_objective(
    params = params,
    targets = targets,
    lazy_dag = ldag,
    n_sim = 200,
    seed = 123,
    mode = "loss"
  )

  expect_equal(obj, sum(residuals^2), tolerance=1e-12)
})

test_that("custom objectives receive scaled residuals", {

  ldag <- .make_two_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 1, scale = 0.25),
    mean_Z = .mean_target("Z", value = -1, scale = 2)
  )

  params <- c(mu_X = 0.2, mu_Z = -0.3)

  achieved <- estimate_targets(
    lazy_dag = ldag,
    params = params,
    targets = targets,
    n_sim = 200,
    seed = 123
  )

  residuals <- (achieved - c(mean_X = 1, mean_Z = -1)) /
    c(mean_X = 0.25, mean_Z = 2)

  absolute_loss <- get_objective(
    params = params,
    targets = targets,
    lazy_dag = ldag,
    n_sim = 200,
    seed = 123,
    objective_fun = function(errors) sum(abs(errors)),
    mode = "loss"
  )

  expect_equal(absolute_loss, sum(abs(residuals)), tolerance=1e-12)
})

test_that("the root objective returns a signed residual", {

  ldag <- .make_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 0.5, scale = 0.25)
  )

  params <- c(mu = 0)

  achieved <- estimate_targets(
    lazy_dag = ldag,
    params = params,
    targets = targets,
    n_sim = 200,
    seed = 123
  )

  residual <- get_objective(
    params = params,
    targets = targets,
    lazy_dag = ldag,
    n_sim = 200,
    seed = 123,
    mode = "root"
  )

  expect_equal(as.numeric(residual), (as.numeric(achieved) - 0.5) / 0.25,
               tolerance=1e-12)
})

test_that("custom objectives must return a finite scalar", {

  ldag <- .make_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 0.5)
  )

  expect_error(
    get_objective(
      params = c(mu = 0),
      targets = targets,
      lazy_dag = ldag,
      n_sim = 100,
      seed = 123,
      objective_fun = function(errors) c(1, 2)
    ), "one finite numeric value"
  )

  expect_error(
    get_objective(
      params = c(mu = 0),
      targets = targets,
      lazy_dag = ldag,
      n_sim = 100,
      seed = 123,
      objective_fun = function(errors) NA_real_
    ), "one finite numeric value"
  )
})

test_that("invalid target scales are rejected", {

  ldag <- .make_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value = 0.5, scale = 0)
  )

  expect_error(
    get_objective(
      params = c(mu = 0),
      targets = targets,
      lazy_dag = ldag,
      n_sim = 100,
      seed = 123,
      mode = "loss"
    ),
    "scales must be finite and strictly positive"
  )
})

test_that("a custom objective disables the uniroot shortcut", {

  ldag <- .make_normal_ldag()

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = list(
      mean_X = .mean_target("X", value = 0.75)
    ),
    params = list(start = 0, range = c(-5, 5)),
    objective_fun = function(errors) sum(errors^2),
    n_sim_run = 300,
    n_sim_eval = 300,
    seed = 123,
    optim_control = .small_optim_control()
  )

  expect_null(fit$uniroot_obj)
  expect_true(!is.null(fit$optim_obj))
  expect_equal(fit$inputs$optim_method, "L-BFGS-B")
})

# ------------------------------------------------------------
# Optimization methods and parameter bounds
# ------------------------------------------------------------

test_that("supported optim methods can be selected", {

  ldag <- .make_normal_ldag()

  methods_without_bounds <- c(
    "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"
  )

  for (method in methods_without_bounds) {

    fit <- calibrate_dag(
      lazy_dag = ldag,
      targets = list(
        mean_X = .mean_target("X", value = 0.5)
      ),
      params = list(start = 0),
      optim_method = method,
      n_sim_run = 100,
      n_sim_eval = 100,
      seed = 123,
      optim_control = .small_optim_control(maxit = 20)
    )

    expect_equal(fit$inputs$optim_method, method)
    expect_null(fit$uniroot_obj)
    expect_true(!is.null(fit$optim_obj))
  }

  # Brent is available for a single parameter with finite bounds.
  fit_brent <- calibrate_dag(
    lazy_dag = ldag,
    targets = list(
      mean_X = .mean_target("X", value = 0.5)
    ),
    params = list(start = 0, range = c(-5, 5)),
    optim_method = "Brent",
    n_sim_run = 100,
    n_sim_eval = 100,
    seed = 123,
    optim_control = .small_optim_control(maxit = 50)
  )

  expect_equal(fit_brent$inputs$optim_method, "Brent")
  expect_null(fit_brent$uniroot_obj)
  expect_true(!is.null(fit_brent$optim_obj))
})

test_that("L-BFGS-B respects finite parameter bounds", {

  fit <- calibrate_dag(
    lazy_dag = .make_normal_ldag(),
    targets = list(
      mean_X = .mean_target("X", value = 3)
    ),
    params = list(
      start = 0,
      range = c(-1, 1)
    ),
    optim_method = "L-BFGS-B",
    n_sim_run = 300,
    n_sim_eval = 300,
    seed = 123,
    optim_control = .small_optim_control(maxit = 200)
  )

  expect_gte(unname(fit$est_params[["mu"]]), 0.99)
  expect_lte(unname(fit$est_params[["mu"]]), 1)
})

test_that("optim methods incompatible with bounds are rejected", {

  expect_error(
    calibrate_dag(
      lazy_dag = .make_normal_ldag(),
      targets = list(
        mean_X = .mean_target("X", value = 0.5)
      ),
      params = list(start = 0, range = c(-5, 5)),
      optim_method = "BFGS",
      n_sim_run = 100,
      n_sim_eval = 100,
      seed = 123,
      optim_control = .small_optim_control()
    ),
    "does not support parameter bounds"
  )
})

test_that("Brent requires one parameter with finite bounds", {

  expect_error(
    calibrate_dag(
      lazy_dag = .make_normal_ldag(),
      targets = list(
        mean_X = .mean_target("X", value = 0.5)
      ),
      params = list(start = 0),
      optim_method = "Brent",
      n_sim_run = 100,
      n_sim_eval = 100,
      seed = 123,
      optim_control = .small_optim_control()
    ),
    "Brent method requires exactly one parameter"
  )
})

# ------------------------------------------------------------
# Parameter validation
# ------------------------------------------------------------

test_that("invalid parameter specifications are rejected", {

  ldag <- .make_normal_ldag()

  expect_error(
    normalize_params(
      params = list(mu = list()),
      lazy_dag = ldag
    ), "start"
  )

  expect_error(
    normalize_params(
      params = list(other = list(start = 0)),
      lazy_dag = ldag
    ), "Parameter names must match"
  )

  expect_error(
    normalize_params(
      params = list(mu = list(start = 0, range = c(2, -2))),
      lazy_dag = ldag
    ), "range"
  )

  expect_error(
    normalize_params(
      params = list(mu = list(start = 0, scale = 0)),
      lazy_dag = ldag
    ), "scale"
  )
})

test_that("parameter names must match lazy DAG placeholders", {

  ldag <- .make_two_normal_ldag()

  expect_error(
    calibrate_dag(
      lazy_dag = ldag,
      targets = list(
        mean_X = .mean_target("X", value = 0.5),
        mean_Z = .mean_target("Z", value = -0.5)
      ),
      params = list(
        mu_X = list(start = 0),
        incorrect_name = list(start = 0)
      ),
      n_sim_run = 100,
      n_sim_eval = 100,
      seed = 123,
      optim_control = .small_optim_control()
    ), "Parameter names must match"
  )
})

# ------------------------------------------------------------
# Reproducibility and user-defined evaluation functions
# ------------------------------------------------------------

test_that("target estimation is reproducible for a fixed seed", {

  ldag <- .make_normal_ldag()
  targets <- list(
    mean_X = .mean_target("X", value = 0.5)
  )

  out1 <- estimate_targets(
    lazy_dag = ldag,
    params = c(mu = 0.2),
    targets = targets,
    n_sim = 300,
    seed = 123
  )

  out2 <- estimate_targets(
    lazy_dag = ldag,
    params = c(mu = 0.2),
    targets = targets,
    n_sim = 300,
    seed = 123
  )

  expect_identical(out1, out2)
})

test_that("target functions can use fitted model statistics", {

  ldag <- lazy_dag(
    empty_dag() +
      node("X", type="rnorm") +
      node("A", type="binomial", formula= ~ param(intercept_A) + X * 0.7) +
      node("Y", type="binomial", formula= ~ param(intercept_Y) + A * 0.5 +
             X * 0.3)
  )

  eval_bias <- function(data) {
    unadjusted <- glm(Y ~ A, family="binomial", data=data)
    adjusted <- glm(Y ~ A + X, family="binomial", data=data)

    unname(coef(unadjusted)[["ATRUE"]] - coef(adjusted)[["ATRUE"]])
  }

  targets <- list(
    bias = list(
      target = 0.5,
      eval_fun = eval_bias
    )
  )

  out <- estimate_targets(
    lazy_dag = ldag,
    params = c(intercept_A = 0, intercept_Y = 0),
    targets = targets,
    n_sim = 2000,
    seed = 123
  )

  expect_length(out, 1)
  expect_true(is.numeric(out) && is.finite(out))
})

test_that("three targets, three parameters", {

  eval_bias <- function(data) {
    fit_unadjusted <- glm(Y ~ A, family="binomial", data=data)
    fit_adjusted <- glm(Y ~ A + X, family="binomial", data=data)
    bias <- coef(fit_unadjusted)[["ATRUE"]] - coef(fit_adjusted)[["ATRUE"]]
    return(bias)
  }

  # list of lists defining the marginal targets
  targets <- list(
    exposure_prevalence = list(
      target = 0.5,
      eval_fun = function(data) {mean(data$A)}
    ),
    outcome_prevalence = list(
      target = 0.2,
      eval_fun = function(data) {mean(data$Y)}
    ),
    bias = list(
      target = 0.5,
      eval_fun = eval_bias
    )
  )

  # list of lists defining the parameter ranges and start values
  params <- list(
    Aintercept = list(start = 0, range = c(-5, 5)),
    Yintercept = list(start = 0, range = c(-5, 5)),
    betaYX = list(start = 0, range = c(-5, 5))
  )

  # a lazy dag with three unspecified parameters
  ldag <- lazy_dag(
    empty_dag() +
      node("X", type="rnorm") +
      node("A", type="binomial", formula = ~ param(Aintercept) + X*log(3)) +
      node("Y", type="binomial", formula = ~ param(Yintercept) + A*log(0.6) +
             X*param(betaYX))
  )

  cdad <- calibrate_dag(ldag, targets=targets, params=params, n_sim_run=1000,
                        n_sim_eval=3000,
                        seed=3245, optim_control = list(
                          maxit = 100,
                          trace = 0,
                          parscale = c(1, 1, 1),
                          ndeps = c(0.05, 0.05, 0.05),
                          factr = 100
                        ))

  expect_s3_class(cdad, "calibrated_DAG")
})

test_that("more parameters than targets", {

  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=param(p)) +
      node("B", type="binomial", formula= ~ -2 + A*param(betaA)) +
      node("C", type="gaussian", formula= ~ param(intercept) + A*2 +
             B*param(betaB), error=1)
  )

  # list of lists defining the marginal targets
  targets <- list(
    A_prevalence = list(
      target = 0.3,
      eval_fun = function(data) {mean(data$A)}
    ),
    C_mean = list(
      target = 20,
      eval_fun = function(data) {mean(data$C)}
    )
  )

  params <- list(
    p = list(start = 0, range = c(0, 1)),
    betaA = list(start = 0, range = c(-5, 5)),
    intercept = list(start = 0, range = c(-50, 50)),
    betaB = list(start = 0, range = c(-5, 5))
  )

  out <- calibrate_dag(lazy_dag=ldag, targets=targets, params=params,
                       optim_control=list(trace=0), n_sim_run=1000,
                       n_sim_eval=4000)

  expect_s3_class(out, "calibrated_DAG")
})

test_that("warning with more targets than parameters", {

  ldag <- lazy_dag(
    empty_dag() +
      node("A", type="rbernoulli", p=0.5) +
      node("B", type="binomial", formula= ~ -2 + A*1.2) +
      node("C", type="gaussian", formula= ~ param(intercept) + A*2 +
             B*3.4, error=1)
  )

  # list of lists defining the marginal targets
  targets <- list(
    A_prevalence = list(
      target = 0.3,
      eval_fun = function(data) {mean(data$A)}
    ),
    C_mean = list(
      target = 20,
      eval_fun = function(data) {mean(data$C)}
    )
  )

  params <- list(
    intercept = list(start = 0, range = c(-50, 50))
  )

  expect_warning(
    calibrate_dag(lazy_dag=ldag, targets=targets, params=params,
                  optim_control=list(trace=0), n_sim_run=1000,
                  n_sim_eval=4000),
    paste0("There are 2 specified target values but only 1 free parameter(s).",
           " Optimization will likely be impossible."), fixed=TRUE
  )
})

test_that("simulation from final DAG is possible", {

  ldag <- .make_normal_ldag()

  targets <- list(
    mean_X = .mean_target("X", value=1.25)
  )

  # flat, non-nested specification for one parameter
  params <- list(
    start = 0,
    range = c(-5, 5)
  )

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    n_sim_run = 300,
    n_sim_eval = 300,
    seed = 123,
    optim_control = .small_optim_control()
  )

  data <- sim_from_dag(fit$dag, n_sim=10)

  expect_true(is.data.table(data))
  expect_equal(nrow(data), 10)
})

test_that("works with sim_discrete_time()", {

  ldag <- lazy_dag(
    empty_dag() +
      node("U", type="rnorm") +
      node_td("Y", type="time_to_event", prob_fun=param(p),
              event_duration=1)
  )

  targets <- list(
    mean_events = list(target = 5, eval_fun = function(x) {
      n_id <- x[, .(n_events = sum(Y)), by=.id]
      mean(n_id$n_events)
    })
  )

  params <- list(start = 0.0000001, range = c(0.000000000001, 0.9))

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    n_sim_run = 100,
    n_sim_eval = 100,
    seed = 1324,
    data_format = "start_stop",
    max_t = 50
  )

  expect_s3_class(fit, "calibrated_DAG")
})

test_that("works with sim_discrete_event()", {

  ldag <- lazy_dag(
    empty_dag() +
      node("U", type="rnorm") +
      node_td("Y", type="next_time", prob_fun=param(p),
              event_duration=1)
  )

  targets <- list(
    mean_events = list(target = 5, eval_fun = function(x) {
      n_id <- x[, .(n_events = sum(Y)), by=.id]
      mean(n_id$n_events)
    })
  )

  params <- list(start = 0.01, range = c(0.000000000001, 0.9))

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    n_sim_run = 100,
    n_sim_eval = 100,
    seed = 1324,
    max_t = 50,
    censor_at_max_t = TRUE,
    target_event = "Y"
  )

  expect_s3_class(fit, "calibrated_DAG")
})

test_that("works with networks", {

  gen_network <- function(n_sim) {
    igraph::sample_gnm(n=n_sim, m=30)
  }

  ldag <- lazy_dag(
    dag <- empty_dag() +
      network("Net1", net=gen_network) +
      node("A", type="rnorm") +
      node("Y", type="gaussian",
           formula= ~ -2 + net(mean(A), na=0)*param(net_eff),
           error=1.5)
  )

  targets <- list(
    net_eff = list(target = -22, eval_fun = function(x) {mean(x$Y)})
  )

  params <- list(start = 0, range = c(-5000, 5000))

  fit <- calibrate_dag(
    lazy_dag = ldag,
    targets = targets,
    params = params,
    seed = 12344,
    n_sim_run = 100,
    n_sim_eval = 200
  )

  expect_s3_class(fit, "calibrated_DAG")
})

test_that("error with wrong lazy_dag argument", {

  dag <- empty_dag() +
    node("A", type="rnorm")

  # mistakenly supplied normal DAG
  expect_error(calibrate_dag(dag), paste0("'lazy_dag' should be a `lazy_DAG` ",
                                          "object, created using lazy_dag(), ",
                                          "not a regular `DAG` object."),
               fixed=TRUE)

  # mistakenly supplied something completely wrong
  expect_error(calibrate_dag(10), paste0("'lazy_dag' must be a `lazy_DAG` ",
                                         "object created using the ",
                                         "lazy_dag() function."), fixed=TRUE)
})
