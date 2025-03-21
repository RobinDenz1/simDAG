
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

## extracts random effects and random slope syntax from a formula string
extract_mixed_terms <- function(formula) {
  regmatches(formula,
             gregexpr("(?<![a-zA-Z0-9])\\(.*?\\)", formula, perl=TRUE))[[1]]
}

## same as gsub() with fixed=TRUE, but allowing a character vector in
## the pattern argument
str_replace_all <- function(string, replace) {
  cleaned <- string
  for (i in seq_len(length(replace))) {
    cleaned <- gsub(replace[i], "", cleaned, fixed=TRUE)
  }
  return(cleaned)
}

## cleans up a formula string if there are too many + signs, usually
## because of the removal of random effects and slope terms
remove_mistaken_plus <- function(formula) {
  formula <- gsub("(\\+)\\1+", "\\1", formula)

  if (endsWith(formula, "+")) {
    formula <- substr(formula, 1, nchar(formula)-1)
  }
  if (startsWith(formula, "+")) {
    formula <- substr(formula, 2, nchar(formula))
  }
  return(formula)
}
