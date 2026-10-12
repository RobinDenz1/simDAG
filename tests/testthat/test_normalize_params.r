
# Minimal object: normalize_params() only needs lazy_dag$params
.make_normalize_params_ldag <- function(param_names) {
  structure(
    list(params = param_names),
    class = "lazy_dag"
  )
}

# Helper for checking exact error messages
.expect_normalize_error <- function(expr, expected) {

  expr <- substitute(expr)

  actual <- tryCatch(
    eval(expr, envir = parent.frame()),
    error = function(e) e
  )

  if (!inherits(actual, "error")) {
    stop("Expected an error, but no error was thrown.", call. = FALSE)
  }

  expect_identical(
    conditionMessage(actual),
    expected
  )

  invisible(actual)
}

# ------------------------------------------------------------
# Valid parameter specifications
# ------------------------------------------------------------

test_that("nested lists and named vectors are supported", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  # Parameters supplied in reverse order; beta uses a nested list,
  # while alpha uses a named numeric vector.
  params <- list(
    beta = list(start = 2),
    alpha = c(start = 1)
  )

  out <- normalize_params(params, ldag)

  expect_named(out, c("alpha", "beta"))
  expect_identical(out$alpha, list(start = 1))
  expect_identical(out$beta, list(start = 2))
})

test_that("a flat list is accepted for one parameter", {

  ldag <- .make_normalize_params_ldag("mu")

  params <- list(
    start = 1,
    range = c(-5, 5),
    scale = 0.5
  )

  out <- normalize_params(params, ldag)

  expect_named(out, "mu")
  expect_identical(
    out,
    list(
      mu = list(
        start = 1,
        range = c(-5, 5),
        scale = 0.5
      )
    )
  )
})

test_that("a flat named vector is accepted for one parameter", {

  ldag <- .make_normalize_params_ldag("mu")

  out <- normalize_params(
    params = c(start = 1),
    lazy_dag = ldag
  )

  expect_identical(
    out,
    list(mu = list(start = 1))
  )
})

test_that("nested named vectors are converted to lists", {

  ldag <- .make_normalize_params_ldag("alpha")

  out <- normalize_params(
    params = list(alpha = c(start = 1, scale = 2)),
    lazy_dag = ldag
  )

  expect_identical(
    out,
    list(alpha = list(start = 1, scale = 2))
  )
})

test_that("range and scale may be omitted", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  out <- normalize_params(
    params = list(
      alpha = list(start = 0),
      beta = list(start = 1)
    ),
    lazy_dag = ldag
  )

  expect_identical(
    out,
    list(
      alpha = list(start = 0),
      beta = list(start = 1)
    )
  )
})

test_that("infinite parameter bounds are accepted", {

  ldag <- .make_normalize_params_ldag("alpha")

  out <- normalize_params(
    params = list(
      alpha = list(start = 0, range = c(-Inf, Inf))
    ),
    lazy_dag = ldag
  )

  expect_identical(
    out$alpha$range,
    c(-Inf, Inf)
  )
})

# ------------------------------------------------------------
# Invalid top-level parameter specifications
# ------------------------------------------------------------

test_that("params must be a named list of parameter specifications", {

  ldag <- .make_normalize_params_ldag("alpha")

  expected <- "`params` must be a named list of parameter specifications."

  # Unnamed list
  .expect_normalize_error(
    normalize_params(list(list(start = 0)), ldag),
    expected
  )

  # Atomic vector without a "start" name
  .expect_normalize_error(
    normalize_params(c(alpha = 0), ldag),
    expected
  )

  # Duplicate parameter names
  duplicate_names <- setNames(
    list(
      list(start = 0),
      list(start = 1)
    ),
    c("alpha", "alpha")
  )

  .expect_normalize_error(
    normalize_params(duplicate_names, ldag),
    expected
  )
})

test_that("flat specifications require exactly one free parameter", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  .expect_normalize_error(
    normalize_params(
      params = list(start = 0),
      lazy_dag = ldag
    ),
    paste0(
      "A non-nested parameter specification requires exactly ",
      "one free parameter."
    )
  )
})

# ------------------------------------------------------------
# Parameter name validation
# ------------------------------------------------------------

test_that("missing parameter names produce the correct error", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  .expect_normalize_error(
    normalize_params(
      params = list(alpha = list(start = 0)),
      lazy_dag = ldag
    ),
    paste0(
      "Parameter names must match the placeholders in `lazy_dag`.",
      "\nMissing: beta"
    )
  )
})

test_that("unknown parameter names produce the correct error", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  .expect_normalize_error(
    normalize_params(
      params = list(
        alpha = list(start = 0),
        beta = list(start = 1),
        gamma = list(start = 2)
      ),
      lazy_dag = ldag
    ),
    paste0(
      "Parameter names must match the placeholders in `lazy_dag`.",
      "\nUnknown: gamma"
    )
  )
})

test_that("missing and unknown parameter names are both reported", {

  ldag <- .make_normalize_params_ldag(c("alpha", "beta"))

  .expect_normalize_error(
    normalize_params(
      params = list(
        alpha = list(start = 0),
        gamma = list(start = 2)
      ),
      lazy_dag = ldag
    ),
    paste0(
      "Parameter names must match the placeholders in `lazy_dag`.",
      "\nMissing: beta",
      "\nUnknown: gamma"
    )
  )
})

# ------------------------------------------------------------
# Starting value validation
# ------------------------------------------------------------

test_that("a missing start value is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(list(alpha = list()), ldag),
    "Parameter `alpha` must have a finite numeric `start` value."
  )
})

test_that("a non-numeric start value is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (start in list("zero", TRUE)) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = start)),
        ldag
      ),
      "Parameter `alpha` must have a finite numeric `start` value."
    )
  }
})

test_that("a start value must be a single number", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(
      list(alpha = list(start = c(0, 1))),
      ldag
    ),
    "Parameter `alpha` must have a finite numeric `start` value."
  )
})

test_that("a non-finite start value is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (start in list(Inf, -Inf, NaN, NA_real_)) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = start)),
        ldag
      ),
      "Parameter `alpha` must have a finite numeric `start` value."
    )
  }
})

# ------------------------------------------------------------
# Parameter range validation
# ------------------------------------------------------------

test_that("a non-numeric range is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(
      list(alpha = list(start = 0, range = c("a", "b"))),
      ldag
    ),
    paste0(
      "`range` for parameter `alpha` must contain two ",
      "increasing numeric bounds."
    )
  )
})

test_that("a range must contain exactly two values", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (range in list(numeric(0), 1, c(-1, 0, 1))) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = 0, range = range)),
        ldag
      ),
      paste0(
        "`range` for parameter `alpha` must contain two ",
        "increasing numeric bounds."
      )
    )
  }
})

test_that("a range containing NA is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(
      list(alpha = list(start = 0, range = c(NA_real_, 1))),
      ldag
    ),
    paste0(
      "`range` for parameter `alpha` must contain two ",
      "increasing numeric bounds."
    )
  )
})

test_that("range bounds must be strictly increasing", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (range in list(c(1, 1), c(2, 1))) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = 0, range = range)),
        ldag
      ),
      paste0(
        "`range` for parameter `alpha` must contain two ",
        "increasing numeric bounds."
      )
    )
  }
})

# ------------------------------------------------------------
# Parameter scale validation
# ------------------------------------------------------------

test_that("a non-numeric scale is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(
      list(alpha = list(start = 0, scale = "large")),
      ldag
    ),
    paste0(
      "`scale` for parameter `alpha` must be finite and ",
      "strictly positive."
    )
  )
})

test_that("a scale must be a single number", {

  ldag <- .make_normalize_params_ldag("alpha")

  .expect_normalize_error(
    normalize_params(
      list(alpha = list(start = 0, scale = c(1, 2))),
      ldag
    ),
    paste0(
      "`scale` for parameter `alpha` must be finite and ",
      "strictly positive."
    )
  )
})

test_that("a non-finite scale is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (scale in list(Inf, -Inf, NaN, NA_real_)) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = 0, scale = scale)),
        ldag
      ),
      paste0(
        "`scale` for parameter `alpha` must be finite and ",
        "strictly positive."
      )
    )
  }
})

test_that("a zero or negative scale is rejected", {

  ldag <- .make_normalize_params_ldag("alpha")

  for (scale in c(0, -1)) {

    .expect_normalize_error(
      normalize_params(
        list(alpha = list(start = 0, scale = scale)),
        ldag
      ),
      paste0(
        "`scale` for parameter `alpha` must be finite and ",
        "strictly positive."
      )
    )
  }
})
