
## sample from different user-specified distributions in user-defined
## strata of covariates
#' @export
node_conditional_distr <- function(data, parents, distr, default_distr=NULL,
                                   default_distr_args=list(),
                                   default_val=NA_real_, coerce2numeric=TRUE) {

  out <- rep(default_val, nrow(data))

  # create interaction of parents if needed
  if (length(parents) > 1) {
    dep_var <- interaction(data[, parents, with=FALSE])
  } else {
    dep_var <- data[[parents]]
  }

  # levels of the dependent variable
  dep_levels <- as.character(unique(dep_var))

  # levels with defined distributions in distr list
  dep_levels_def <- names(distr)

  # sample from corresponding distributions
  for (i in seq_len(length(dep_levels_def))) {

    # extract fun
    dist_fun <- get(distr[[dep_levels_def[i]]][[1]])

    # prepare args list
    distr[[dep_levels_def[i]]][[1]] <- NULL
    distr[[dep_levels_def[i]]]$n <- sum(dep_var==dep_levels_def[i])

    # call relevant distribution function
    strata_values <- do.call(dist_fun,args=distr[[dep_levels_def[i]]])

    if (coerce2numeric) {
      strata_values <- as.numeric(strata_values)
    }

    out[dep_var==dep_levels_def[i]] <- strata_values
  }

  # sample from default distribution, or set to default value directly
  # for all strata not included in distr list
  if (length(dep_levels_def) < length(dep_levels) && !is.null(default_distr)) {

    strat_default_ind <- !dep_var %in% dep_levels_def

    default_distr_args$n <- sum(strat_default_ind)
    out[strat_default_ind] <- do.call(default_distr, args=default_distr_args)
  }

  return(out)
}
