
.make_calibrated_DAG_snapshot <- function(
    variant = c(
      "standard",
      "failed",
      "root",
      "fallbacks",
      "stored_method"
    )) {

  variant <- match.arg(variant)

  if (variant == "root") {

    params <- list(
      mu = list(start = 0, range = c(-2, 2))
    )

    targets <- list(
      mean_x = list(
        target = 0.25,
        scale = 0.1,
        eval_fun = function(data) mean(data$x)
      )
    )

    return(structure(
      list(
        dag = NULL,
        inputs = list(
          params = params,
          targets = targets,
          optim_control = list(parscale = NULL),
          optim_method = NULL,
          n_sim_run = 10000,
          n_sim_eval = 100000,
          seed = 1234
        ),
        est_params = c(mu = 0.25),
        optim_obj = NULL,
        uniroot_obj = list(
          root = 0.25,
          f.root = 0.001,
          iter = 7L
        ),
        target_eval = c(mean_x = 0.251)
      ),
      class = "calibrated_DAG"
    ))
  }

  params <- list(
    alpha = list(start = 0, range = c(-2, 2)),
    beta = list(start = 0.5)
  )

  targets <- list(
    prevalence = list(
      target = 0.4,
      scale = 0.1,
      eval_fun = function(data) mean(data$x)
    ),
    mean_y = list(
      target = 1,
      eval_fun = function(data) mean(data$y)
    )
  )

  est_params <- c(beta = -0.1, alpha = 0.25)
  target_eval <- c(prevalence = 0.401, mean_y = 0.98)

  optim_method <- NULL
  parscale <- c(beta = 3, alpha = 2)
  convergence <- 0L
  message <- NULL
  counts <- c(`function` = 12L, gradient = 4L)

  if (variant == "failed") {
    optim_method <- "L-BFGS-B"
    convergence <- 1L
    message <- "iteration limit reached"
    counts <- NULL
  }

  if (variant == "fallbacks") {
    names(est_params) <- NULL
    names(targets) <- NULL
    targets[[1]]$scale <- NULL
    parscale <- NULL
    target_eval <- c(NA_real_, Inf)
  }

  if (variant == "stored_method") {
    optim_method <- "BFGS"
    parscale <- c(alpha = 2)
    counts <- 5L
  }

  structure(
    list(
      dag = NULL,
      inputs = list(
        params = params,
        targets = targets,
        optim_control = list(parscale = parscale),
        optim_method = optim_method,
        n_sim_run = 10000,
        n_sim_eval = 100000,
        seed = 1234
      ),
      est_params = est_params,
      optim_obj = list(
        convergence = convergence,
        value = 0.123,
        message = message,
        counts = counts
      ),
      uniroot_obj = NULL,
      target_eval = target_eval
    ),
    class = "calibrated_DAG"
  )
}

test_that("standard calibrated DAG output", {
  x <- .make_calibrated_DAG_snapshot("standard")

  expect_snapshot({
    print(x)
    summary(x)
  })
})

test_that("unsuccessful optimization output", {
  x <- .make_calibrated_DAG_snapshot("failed")

  expect_snapshot({
    print(x)
    summary(x)
  })
})

test_that("root-finding output", {
  x <- .make_calibrated_DAG_snapshot("root")

  expect_snapshot({
    print(x)
    summary(x)
  })
})

test_that("fallback and non-finite output", {
  x <- .make_calibrated_DAG_snapshot("fallbacks")

  expect_snapshot({
    print(x)
    summary(x)
  })
})

test_that("explicit optimization method and scalar evaluation count", {
  x <- .make_calibrated_DAG_snapshot("stored_method")

  expect_snapshot({
    print(x)
    summary(x)
  })
})
