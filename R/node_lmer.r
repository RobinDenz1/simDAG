
## generates data from a mixed model using the simr package
# NOTE: this function is not exported, because it would not fit into the
#       current naming scheme of the nodes. Instead, it is automatically
#       called whenever the user puts random effects or slopes in an
#       enhanced formula
node_lmer <- function(data, formula, betas, intercept, family,
                      var_corr, error) {

  requireNamespace("simr")

  # create fake lmerMod object
  if (family=="gaussian") {
    lmer_obj <- simr::makeLmer(formula=formula, fixef=c(intercept, betas),
                               VarCorr=var_corr, data=data, sigma=error)
  } else {
    lmer_obj <- simr::makeGlmer(formula=formula, fixef=c(intercept, betas),
                                VarCorr=var_corr, data=data, family=family)
  }

  # simulate from it
  out <- simr::doSim(lmer_obj)

  return(out)
}

## put together formula for lmer model using fixed and random effects parts
## extracted from the enhanced formula
get_formula_for_node_lmer <- function(formula_parts, mixed_terms) {

  formula <- stats::as.formula(paste0(
    "...PLACEHOLDER... ~ `",
    paste0(formula_parts, collapse="` + `"),
    "` + ",
    paste0(mixed_terms, collapse=" + ")
  ))

  return(formula)
}

## check if an enhanced formula contains a random effect or random slope
has_mixed_terms <- function(formula) {
  grepl("|", formula, fixed=TRUE)
}

## check if a node type supports mixed model terms
supports_mixed_terms <- function(node_type) {
  (!is.function(node_type) &&
     node_type %in% c("gaussian", "binomial", "poisson")) ||
    (is.function(node_type) && (is_same_object(node_type, node_gaussian) ||
                                  is_same_object(node_type, node_binomial) ||
                                  is_same_object(node_type, node_poisson)))
}
