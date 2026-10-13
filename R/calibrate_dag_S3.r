
## S3 summary method for calibrated_DAG objects
#' @export
summary.calibrated_DAG <- function(object, print=TRUE, ...) {

  inputs <- object$inputs
  params <- inputs$params
  targets <- inputs$targets
  param_names <- names(params)

  ## Parameter summary
  estimates <- object$est_params

  if (is.null(names(estimates))) {
    names(estimates) <- param_names
  }

  estimates <- estimates[param_names]
  starts <- vapply(X=params, FUN=function(x) {x$start}, FUN.VALUE=numeric(1))

  lower <- vapply(
    X=params,
    FUN=function(x) {if (is.null(x$range)) -Inf else x$range[1]},
    FUN.VALUE=numeric(1)
  )

  upper <- vapply(
    X=params,
    FUN=function(x) {if (is.null(x$range)) Inf else x$range[2]},
    FUN.VALUE=numeric(1)
  )

  # effective optimizer scaling
  method <- inputs$optim_method

  if (is.null(method)) {
    if (!is.null(object$uniroot_obj)) {
      method <- "uniroot"
    } else {
      method <- "L-BFGS-B"
    }
  }

  if (method == "uniroot") {
    parscale <- rep(NA_real_, length(param_names))
  } else {
    parscale <- inputs$optim_control$parscale

    if (is.null(parscale)) {
      parscale <- rep(NA_real_, length(param_names))
    } else if (!is.null(names(parscale)) &&
               all(param_names %in% names(parscale))) {
      parscale <- parscale[param_names]
    }

    parscale <- as.numeric(parscale)

    if (length(parscale) != length(param_names)) {
      parscale <- rep(NA_real_, length(param_names))
    }
  }

  parameter_summary <- data.frame(
    parameter = param_names,
    estimate = as.numeric(estimates),
    start = as.numeric(starts),
    lower = as.numeric(lower),
    upper = as.numeric(upper),
    parscale = parscale,
    row.names = NULL
  )

  # target summary
  target_names <- names(targets)

  if (is.null(target_names)) {
    target_names <- paste0("Target ", seq_along(targets))
  }

  desired <- vapply(X=targets, FUN=function(x) {as.numeric(x$target)},
                    FUN.VALUE=numeric(1))

  scales <- vapply(
    X=targets,
    FUN=function(x) {if (is.null(x$scale)) 1 else as.numeric(x$scale)},
    FUN.VALUE=numeric(1)
  )

  achieved <- as.numeric(object$target_eval)
  errors <- achieved - desired

  target_summary <- data.frame(
    target = target_names,
    desired = desired,
    achieved = achieved,
    error = errors,
    scaled_error = errors / scales,
    row.names = NULL
  )

  # optimization diagnostics
  if (!is.null(object$uniroot_obj)) {

    opt <- object$uniroot_obj

    optimization <- list(
      optimizer = "uniroot",
      method = "uniroot",
      converged = TRUE,
      code = NA_integer_,
      objective = NULL,
      root_residual = opt$f.root,
      iterations = opt$iter,
      message = NULL
    )

  } else {

    opt <- object$optim_obj

    optimization <- list(
      optimizer = inputs$optimizer,
      method = method,
      converged = opt$convergence == 0,
      code = opt$convergence,
      objective = opt$value,
      root_residual = NULL,
      iterations = opt$counts,
      message = opt$message
    )
  }

  # assemble summary
  out <- list(
    n_parameters = length(estimates),
    n_targets = length(targets),
    parameters = parameter_summary,
    targets = target_summary,
    optimization = optimization,
    n_sim_run = inputs$n_sim_run,
    n_sim_eval = inputs$n_sim_eval
  )

  class(out) <- "summary_calibrated_DAG"

  if (print) {
    print.summary_calibrated_DAG(out)
  } else {
    out
  }
}

## S3 print method for summary_calibrated_DAG objects
print.summary_calibrated_DAG <- function(x, digits=4, ...) {

  line <- strrep("\u2500", 60)

  cat(line, sep = "")
  cat("\nCalibrated DAG\n")
  cat(line, sep = "")

  # parameters
  cat("\nParameters (", x$n_parameters, ")\n", sep="")
  print(x$parameters, row.names=FALSE, digits=digits)

  # targets
  cat("\nTargets (", x$n_targets, ")\n", sep="")
  print(x$targets, row.names=FALSE, digits=digits)

  # optimization
  opt <- x$optimization

  cat("\nOptimization\n")

  if (opt$optimizer != "uniroot") {
    cat("  Optimizer:    ", opt$optimizer, "()\n", sep = "")
  }

  cat("  Method:       ", opt$method, "\n", sep = "")

  if (opt$converged) {
    cat("  Convergence:  successful\n")
  } else {
    cat("  Convergence:  unsuccessful (code ", opt$code, ")\n", sep="")
  }

  if (!is.null(opt$objective)) {
    cat("  Objective:    ", format(opt$objective, digits=digits),
        "\n", sep="")
  }

  if (!is.null(opt$root_residual)) {
    cat("  Root residual: ", format(opt$root_residual, digits=digits),
        "\n", sep="")
  }

  if (!is.null(opt$iterations)) {
    if (length(opt$iterations) == 1L) {
      cat("  Iterations:   ", opt$iterations, "\n", sep="")
    } else {
      cat("  Evaluations:  ", paste(names(opt$iterations), opt$iterations,
                                    collapse=", "), "\n", sep="")
    }
  }

  if (!is.null(opt$message)) {
    cat("  Message:      ", opt$message, "\n", sep="")
  }

  cat("  Simulations:  ", format(x$n_sim_run, big.mark=","),
      " optimization; ", format(x$n_sim_eval, big.mark=","),
      " final evaluation\n", sep="")
  cat(line, sep = "")
  cat("\n")
  invisible(x)
}

## S3 print method for calibrated_DAG objects
#' @export
print.calibrated_DAG <- function(x, ...) {

  s <- summary(x, print=FALSE)
  opt <- s$optimization

  cat("\nCalibrated DAG object\n")
  cat(strrep("-", 40), "\n", sep="")

  cat("  Parameters: ", s$n_parameters, "\n", sep="")
  cat("  Targets:    ", s$n_targets, "\n", sep="")

  # optimization status
  if (opt$converged) {
    status <- "converged"
  } else {
    status <- paste0("not converged (code ", opt$code, ")")
  }

  if (opt$optimizer != "uniroot") {
    cat("  Optimizer:  ", opt$optimizer, "()\n", sep = "")
  }

  cat("  Method:     ", opt$method, " (", status, ")\n", sep="")

  # largest absolute scaled target error
  errors <- s$targets$scaled_error

  if (length(errors) > 0L && all(is.finite(errors))) {
    max_error <- max(abs(errors))
  } else {
    max_error <- NA_real_
  }

  if (is.finite(max_error)) {
    max_error <- format(max_error, digits = 4)
  } else {
    max_error <- "unavailable"
  }

  cat("  Max |scaled target error|: ", max_error, "\n", sep="")

  # simulation sizes
  cat("  Simulations: ", format(s$n_sim_run, big.mark=","),
      " (optimization); ", format(s$n_sim_eval, big.mark=","),
      " (final evaluation)\n", sep="")
  cat("\nUse summary(x) for parameter estimates, target results, ",
      "and optimization details.\n", sep = "")

  invisible(x)
}
