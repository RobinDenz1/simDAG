
## sample from different user-specified distributions in user-defined
## strata of covariates
#' @export
node_conditional_distr <- function(data, parents, distr, coerce2numeric=TRUE) {

  out <- rep(NA_real_, nrow(data))

  # create interaction of parents if needed
  if (length(parents) > 1) {
    dep_var <- interaction(data[, parents, with=FALSE])
  } else {
    dep_var <- data[[parents]]
  }

  # levels of the dependent variable
  dep_levels <- as.character(unique(dep_var))

  # sample from corresponding distributions
  for (i in seq_len(length(dep_levels))) {

    # extract fun
    dist_fun <- get(distr[[dep_levels[i]]][[1]])

    # prepare args list
    distr[[dep_levels[i]]][[1]] <- NULL
    distr[[dep_levels[i]]]$n <- sum(dep_var==dep_levels[i])

    # call relevant distribution function
    strata_values <- do.call(dist_fun,args=distr[[dep_levels[i]]])

    if (coerce2numeric) {
      strata_values <- as.numeric(strata_values)
    }

    out[dep_var==dep_levels[i]] <- strata_values
  }

  return(out)
}
