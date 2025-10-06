
## simulate from a zero-inflated regression model, where the count
## part is simulated using either a poisson or a negative-binomial regression
## and the zero inflated part is simulated using a logistic regression
#' @export
node_zeroinfl <- function(data, parents, parents_count, parents_zero,
                          formula_count, formula_zero, betas_count, betas_zero,
                          intercept_count, intercept_zero,
                          family_count="poisson", theta,
                          link_count, link_zero="logit",
                          var_corr_count, var_corr_zero) {

  # simulate counting process part first
  args <- list()
  if (!missing(var_corr_count)) {
    args$var_corr <- var_corr_count
  }

  if (!missing(formula_count)) {
    formula_count <- sanitize_formula(formula_count)
    args <- args_from_formula(args=args, formula=formula_count,
                              node_type=family_count)
    args$data <- data_for_formula(data=data, args=args)
    args$mixed_terms <- NULL
  } else {
    args <- list(data=data,
                 parents=parents_count,
                 betas=betas_count,
                 intercept=intercept_count)
  }

  # add theta for negative binomial regression
  if (!missing(theta)) {
    args$theta <- theta
  }

  # add link function if specified
  if (!missing(link_count)) {
    args$link <- link_count
  }

  # call the respective node function
  if (family_count=="poisson") {
    out_count <- do.call(node_poisson, args=args)
  } else if (family_count=="negative_binomial") {
    out_count <- do.call(node_negative_binomial, args=args)
  }

  # simulate zero part next
  args <- list()
  if (!missing(var_corr_zero)) {
    args$var_corr <- var_corr_zero
  }

  if (!missing(formula_zero)) {
    formula_zero <- sanitize_formula(formula_zero)
    args <- args_from_formula(args=args, formula=formula_zero,
                              node_type="binomial")
    args$data <- data_for_formula(data=data, args=args)
    args$mixed_terms <- NULL
  } else {
    args <- list(data=data,
                 parents=parents_zero,
                 betas=betas_zero,
                 intercept=intercept_zero)
  }

  args$link <- link_zero
  out_zero <- do.call(node_binomial, args=args)

  # put both parts together
  out <- out_zero * out_count
  return(out)
}

## check if a node is a zero-inflated node
is_zeroinfl_node <- function(node) {
  (is.function(node) && is_same_object(node, node_zeroinfl)) ||
  (is.character(node) && node=="zeroinfl")
}

## extract a standard parents vector from the arguments of a
## zero-inflated node
parents_from_zeroinfl <- function(...) {

  args <- list(...)

  # parents of count model
  if (!is.null(args$parents_count)) {
    parents_count <- args$parents_count
  } else if (!is.null(args$formula_count)) {
    form <- sanitize_formula(args$formula_count)
    parents_count <- parents_from_formula(formula=form,
                                          node_type=args$family_count)
  } else {
    stop("Either 'parents_count' or 'formula_count' must be specified",
         " when using type='zeroinfl'.", call.=FALSE)
  }

  # parents of zero-inflation model
  if (!is.null(args$parents_zero)) {
    parents_zero <- args$parents_zero
  } else if (!is.null(args$formula_zero)) {
    form <- sanitize_formula(args$formula_zero)
    parents_zero <- parents_from_formula(formula=form,
                                         node_type="binomial")
  } else {
    stop("Either 'parents_zero' or 'formula_zero' must be specified",
         " when using type='zeroinfl'.", call.=FALSE)
  }

  return(unique(c(parents_count, parents_zero)))
}
