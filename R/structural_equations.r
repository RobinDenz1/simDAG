
## given a function object, get its' name
## NOTE: this "fails" if there are two functions that have exactly the same
#        definition in the same environment
extract_function_name <- function(fun) {

  out <- tryCatch({
    env <- environment(fun)

    for (obj_name in ls(env)) {
      if (identical(env[[obj_name]], fun)) {
        return(obj_name)
      }
    }
  }, error=function(err){return(NA_character_)})

  return(out)
}

## creates a string like 1.2 * age + ... for structural equations
get_beta_plus_parents <- function(betas, parents) {
  return(paste0(paste0(betas, "*", parents), collapse=" + "))
}

## concatenate parameters into one string
get_param_str <- function(params, use_names=FALSE) {
  if (use_names) {
    out <- paste0(paste0(names(params), "=", params), collapse=", ")
  } else {
    out <- paste0(params, collapse=", ")
  }
  return(out)
}

## change some distribution functions to default values
get_distr_default <- function(type) {
  if (type=="rnorm") {
    type <- "N"
  } else if (type=="rbernoulli") {
    type <- "Bernoulli"
  } else if (type=="rcategorical") {
    type <- "Multinomial"
  } else if (type=="rconstant") {
    type <- ""
  }
  return(type)
}

## get string for different link functions
#' @importFrom data.table fcase
get_link_str <- function(link) {

  part1 <- fcase(
    link=="identity", "",
    link=="logit", "logit(",
    link=="probit", "pnorm(",
    link=="log", "exp(",
    link=="cloglog", "1 - exp(-exp(",
    link=="cauchit", "pcauchy(",
    link=="inverse", "1 / (",
    link=="sqrt", "(",
    default=""
  )

  part2 <- fcase(
    link=="identity", "",
    link %in% c("logit", "probit", "log", "cauchit", "inverse"), ")",
    link=="cloglog", "))",
    link=="sqrt", ")^2",
    default=""
  )

  out <- list(part1=part1, part2=part2)
  return(out)
}

## get default link function depending on the node type
#' @importFrom data.table fcase
get_link <- function(node) {

  # get link argument
  if (!is.null(node$link)) {
    link_str <- node$link
  } else {
    link_str <- fcase(
      node$type_str=="gaussian", "identity",
      node$type_str=="binomial", "logit",
      node$type_str=="poisson", "log",
      default="identity"
    )
  }

  # get strings needed in structural equations
  out <- get_link_str(link_str)
  return(out)
}

# some cosmetic changes to supplied formula
prep_formula_str_eq <- function(formula) {
  formula <- gsub("~", "", formula, fixed=TRUE)
  formula <- gsub(" * ", "*", formula, fixed=TRUE)
  return(formula)
}

## structural equation for a root node
str_eq_root <- function(node) {

  # define data generation function string
  type_str <- get_distr_default(node$type_str)

  # get param string
  use_names <- !node$type_str %in% c("rnorm", "rbernoulli", "rcategorical",
                                     "rconstant")
  params <- node$params[!names(node$params) %in% c("output", "labels")]
  params <- get_param_str(params, use_names=use_names)

  # put together
  out <- paste0(node$name, " ~ ", type_str, "(", params, ")")

  return(out)
}

## structural equation for binomial child node
str_eq_binomial <- function(node) {

  link <- get_link(node)

  if (!is.null(node$formula) && !is_formula(node$formula)) {
    out <- paste0(node$name, " ~ Bernoulli(", link$part1,
                  prep_formula_str_eq(node$formula), link$part2, ")")
  } else if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ Bernoulli()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)

    if (!is.null(node$return_prob) && node$return_prob) {
      out <- paste0(node$name, " ~ ", link$part1, node$intercept, " + ",
                    beta_eq, link$part2)
    } else {
      out <- paste0(node$name, " ~ Bernoulli(", link$part1,
                    node$intercept, " + ",
                    beta_eq, link$part2, ")")
    }
  }
  return(out)
}

## structural equation for gaussian child node
str_eq_gaussian <- function(node) {

  link <- get_link(node)

  if (!is.null(node$formula) && !is_formula(node$formula)) {
    out <- paste0(node$name, " ~ N(", link$part1,
                  prep_formula_str_eq(node$formula), ", ", node$error,
                  link$part2, ")")
  } else if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ N()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ N(", link$part1, node$intercept, " + ",
                  beta_eq, ", ", node$error, link$part2, ")")
  }
  return(out)
}

## structural equation for poisson node
str_eq_poisson <- function(node) {

  link <- get_link(node)

  if (!is.null(node$formula) && !is_formula(node$formula)) {
    out <- paste0(node$name, " ~ Poisson(", link$part1,
                  prep_formula_str_eq(node$formula), link$part2, ")")
  } else if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ Poisson()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ Poisson(", link$part1, node$intercept,
                  " + ", beta_eq, link$part2, ")")
  }
  return(out)
}

## structural equation for negative binomial node
str_eq_negative_binomial <- function(node) {

  if (!is.null(node$formula) && !is_formula(node$formula)) {
    out <- paste0(node$name, " ~ NegBinomial(",
                  prep_formula_str_eq(node$formula),
                  " + log(", node$theta, "))")
  } else if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ NegBinomial()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ NegBinomial(", node$intercept, " + ", beta_eq,
                  " + log(", node$theta, "))")
  }
  return(out)
}

## structural equation for zero-inflated nodes
str_eq_zeroinfl <- function(node) {

  # count part
  if (!is.null(node$formula_count)) {
    form <- copy(sanitize_formula(node$formula_count))
    args <- list(name=node$name, type=node$family_count,
                 formula=form)
    node_count <- do.call("node", args=args)
  } else {
    args <- list(name=node$name, type=node$family_count,
                 parents=node$parents_count,
                 betas=node$betas_count,
                 intercept=node$intercept_count,
                 var_corr=node$var_corr_count)
    node_count <- do.call("node", args=args)
  }
  out_count <- structural_equation(node_count)
  out_count <- gsub(node$name, paste0(node$name, "[count]"),
                    out_count, fixed=TRUE)

  # zero part
  if (!is.null(node$formula_zero)) {
    form <- copy(sanitize_formula(node$formula_zero))
    args <- list(name=node$name, type="binomial",
                 formula=form)
    node_zero <- do.call("node", args=args)
  } else {
    args <- list(name=node$name, type="binomial",
                 parents=node$parents_zero,
                 betas=node$betas_zero,
                 intercept=node$intercept_zero,
                 var_corr=node$var_corr_zero)
    node_zero <- do.call("node", args=args)
  }
  out_zero <- structural_equation(node_zero)
  out_zero <- gsub(node$name, paste0(node$name, "[zero]"),
                   out_zero, fixed=TRUE)

  return(c(out_count, out_zero))
}

## structural equation for conditional probability nodes
str_eq_conditional_prob <- function(node) {

  if (length(node$probs[[1]]) > 1) {
    gen <- "Multinomial"
  } else {
    gen <- "Bernoulli"
  }

  # defined probabilities
  out <- character(length(node$probs)+1)
  for (i in seq_len(length(node$probs))) {
    probs_i <- paste0(node$probs[[i]], collapse=", ")
    names_i <- paste0(node$parents, "=",
                      strsplit(names(node$probs)[i], ".", fixed=TRUE)[[1]])
    names_i <- paste0(names_i, collapse=" & ")

    out[i] <- paste0(node$name, "(", names_i, ") ~ ", gen, "(", probs_i, ")")
  }

  # defaults
  if (!is.null(node$default_probs)) {
    probs_i <- paste0(node$default_probs, collapse=", ")
    out[length(out)] <- paste0(node$name, "(other) ~ ", gen, "(", probs_i, ")")
  } else {
    if (is.null(node$default_val)) {
      default_val <- formals("node_conditional_prob")$default_val
    } else {
      default_val <- node$default_val
    }
    out[length(out)] <- paste0(node$name, "(other) ~ ", default_val)
  }
  return(out)
}

## structural equation for conditional distribution nodes
str_eq_conditional_distr <- function(node) {

  # defined distributions
  out <- character(length(node$distr)+1)
  for (i in seq_len(length(node$distr))) {

    # distribution function name
    distr_i <- get_distr_default(node$distr[[i]][[1]])

    # arguments to distribution function
    args_i <- node$distr[[i]]
    args_i <- get_param_str(args_i[2:length(args_i)], use_names=TRUE)

    # strata names
    names_i <- paste0(node$parents, "=",
                      strsplit(names(node$distr)[i], ".", fixed=TRUE)[[1]])
    names_i <- paste0(names_i, collapse=" & ")

    # put together
    out_i <- paste0(node$name, "(", names_i, ") ~ ", distr_i, "(", args_i, ")")
    out[i] <- out_i
  }

  # default distribution
  if (!is.null(node$default_distr)) {
    args_i <- get_param_str(node$default_distr_args)
    out[length(out)] <- paste0(node$name, "(other) ~ ",
                               get_distr_default(node$default_distr),
                               "(", args_i, ")")
  } else {
    if (is.null(node$default_val)) {
      default_val <- formals("node_conditional_distr")$default_val
    } else {
      default_val <- node$default_val
    }
    out[length(out)] <- paste0(node$name, "(other) ~ ", default_val)
  }
  return(out)
}

## structural equation for identity nodes
str_eq_identity <- function(node) {
  form_str <- paste0(str_trim(deparse(node$formula)), collapse="")
  form_str <- substr(form_str, 2, nchar(form_str))
  out <- paste0(node$name, " ~ ", form_str)
  return(out)
}

## structural equation for mixture nodes
str_eq_mixture <- function(node) {

  out <- vector(mode="list", length=length(node$distr)/2)
  for (i in seq(1, length(node$distr), 2)) {

    # get structural equations for the regular nodes
    node_str_eq <- structural_equation(node$distr[[i+1]])
    out[[i]] <- gsub(node$distr[[i+1]]$name,
                     paste0(node$name, "[", node$distr[[i]], "]"),
                     node_str_eq, fixed=TRUE)
  }

  return(unlist(out))
}

## structural equation for time-to-event / competing events node
str_eq_time_to_event <- function(node) {

  if (node$type_str=="time_to_event") {
    gen <- "Bernoulli"
  } else if (node$type_str=="competing_events") {
    gen <- "Multinomial"
  }

  if (is.function(node$prob_fun)) {
    # get probability function name
    prob_fun_name <- extract_function_name(node$prob_fun)

    # parse arguments to prob_fun
    prob_fun_args <- names(formals(node$prob_fun))
    prob_fun_args <- prob_fun_args[prob_fun_args != "data"]
    prob_fun_args[prob_fun_args=="sim_time"] <- "t"

    prob_fun_args <- paste0(prob_fun_args, collapse=", ")

    # put together
    if (is.null(node$parents)) {
      out <- paste0(node$name, "(t) ~ ", gen, "(", prob_fun_name, "(",
                    prob_fun_args, "))")
    } else {
      parents <- paste0(paste0(node$parents, "(t)"), collapse=", ")
      if (prob_fun_args=="") {
        out <- paste0(node$name, "(t) ~ ", gen, "(", prob_fun_name, "(",
                      parents, "))")
      } else {
        out <- paste0(node$name, "(t) ~ ", gen, "(", prob_fun_name, "(",
                      prob_fun_args, ", ", parents, "))")
      }
    }

  } else {
    out <- paste0(node$name, "(t) ~ ", gen, "(", node$prob_fun, ")")
  }
  return(out)
}

## structural equation for cox regression based nodes
str_eq_cox <- function(node) {

  if (!is.null(node$formula) && !is_formula(node$formula)) {
    beta_eq <- prep_formula_str_eq(node$formula)
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
  }

  if (node$surv_dist=="weibull") {
    right <- paste0("(-(log(Unif(0, 1))/(", node$lambda, "*exp(",
                    beta_eq, "))))^(1/", node$gamma, ")")
  } else if (node$surv_dist=="exponential") {
    right <- paste0("-(log(Unif(0, 1))/(", node$lambda, "*exp(", beta_eq, ")))")
  }

  out_time <- paste0(node$name, "[T] ~ ", right)

  if (!is.null(node$cens_dist)) {
    out_cens <- paste0(node$name, "[C] ~ ", get_distr_default(node$cens_dist),
                       "(", get_param_str(node$cens_args, use_names=TRUE), ")")
  } else {
    out_cens <- paste0(get_distr_default(node$name), "[C] ~ Inf")
  }

  return(c(out_time, out_cens))
}

## structural equation for nodes based on rsurv package
str_eq_rsurv <- function(node, type) {

  # event time part
  if (!is.null(node$formula) && !is_formula(node$formula)) {
    beta_eq <- prep_formula_str_eq(node$formula)
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
  }

  if (!is.null(node$dist)) {
    base_dist <- node$dist
  } else {
    base_dist <- node$baseline
  }

  time_eq <- paste0(node$name, "[T] ~ ", type, "(", beta_eq, ", dist='",
                    base_dist, "')")

  # censoring part
  if (!is.null(node$cens_dist)) {
    out_cens <- paste0(node$name, "[C] ~ ", get_distr_default(node$cens_dist),
                       "(", get_param_str(node$cens_args, use_names=TRUE), ")")
  } else {
    out_cens <- paste0(get_distr_default(node$name), "[C] ~ Inf")
  }

  return(c(time_eq, out_cens))
}

str_eq_aftreg <- function(node) {
  str_eq_rsurv(node=node, type="aftreg")
}

str_eq_ahreg <- function(node) {
  str_eq_rsurv(node=node, type="ahreg")
}

str_eq_ehreg <- function(node) {
  str_eq_rsurv(node=node, type="ehreg")
}

str_eq_poreg <- function(node) {
  str_eq_rsurv(node=node, type="poreg")
}

str_eq_ypreg <- function(node) {
  str_eq_rsurv(node=node, type="ypreg")
}

## structural equation for custom time-varying nodes
str_eq_td <- function(node) {
  args <- names(node)
  args <- args[!args %in% c("name", "type_str", "type_fun", "parents",
                            "time_varying")]
  args <- paste0(args, collapse=", ")

  if ("sim_time" %in% names(formals(paste0("node_", node$type_str)))) {
    args <- paste0("t, ", args)
  }

  if (is.null(node$parents)) {
    out <- paste0(node$name, "(t) ~ node_", node$type_str, "(", args, ")")
  } else {
    parents <- paste0(paste0(node$parents, "(t)"), collapse=", ")
    out <- paste0(node$name, "(t) ~ node_", node$type_str, "(", parents, ", ",
                  args, ")")
  }
  return(out)
}

## structural equation for custom child nodes
str_eq_child <- function(node) {
  args <- names(node)
  args <- args[!args %in% c("name", "type_str", "type_fun", "parents",
                            "time_varying")]
  args <- paste0(args, collapse=", ")

  parents <- paste0(node$parents, collapse=", ")

  out <- paste0(node$name, " ~ node_", node$type_str, "(", parents, ", ",
                args, ")")
  return(out)
}

## breaks very long structural equations into multiple lines for printing
add_line_breaks <- function(str, char_max=60, pad=NULL) {

  if (nchar(str) <= char_max) {
    return(str)
  }

  # find last + symbol before the n_break character limit
  last_plus <- regexpr("\\+[^\\+]*$", substr(str, 1, char_max))[[1]]

  if (last_plus==-1) {
    return(str)
  }

  # break string into two parts
  part1 <- substr(str, 1, last_plus)
  part2 <- substr(str, last_plus + 1, nchar(str))

  # count characters before first (
  if (is.null(pad)) {
    pad <- regexpr("\\(", part1)[[1]][1]
  }

  # adjust second part
  part2 <- paste0(paste0(rep(" ", pad), collapse=""), part2)

  # if still to long, recursively apply same strategy to second part again
  if (nchar(part2) >= char_max) {
    part2 <- add_line_breaks(str=part2, char_max=char_max, pad=pad)
  }

  # put back together as one string on two lines
  out <- paste0(part1, "\n", part2)

  return(out)
}

## align structural equations on tilde for printing
align_str_equations <- function(str_equations) {
  tilde_pos <- unlist(gregexpr("~", str_equations))
  spaces <- strrep(" ", max(tilde_pos) - tilde_pos)
  return(paste0(spaces, str_equations))
}

## get structural equation e.g. data generation mechanism from a node in
## informative string format
structural_equation <- function(node) {
  ## time-varying nodes
  if (node$time_varying) {
    if (node$type_str %in% c("time_to_event", "competing_events")) {
      out <- str_eq_time_to_event(node)
    } else {
      out <- str_eq_td(node)
    }
  ## child nodes
  } else if (!is.null(node$parents)) {
    if (node$type_str %in% c("gaussian", "binomial", "conditional_prob",
                             "conditional_distr", "multinomial", "poisson",
                             "negative_binomial", "cox", "identity",
                             "mixture", "aftreg", "ahreg", "ehreg",
                             "poreg", "ypreg")) {
      str_eq_fun <- get(paste0("str_eq_", node$type_str))
      out <- str_eq_fun(node)
    } else {
      out <- str_eq_child(node)
    }
  ## root nodes
  } else {
    out <- str_eq_root(node)
  }
  return(out)
}
