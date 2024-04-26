
## given a function object, get its' name
## NOTE: this "fails" if there are two functions that have exactly the same
#        definition in the same environment
extract_function_name <- function(fun) {
  env <- environment(fun)

  for (obj_name in ls(env)) {
    if (identical(env[[obj_name]], fun)) {
      return(obj_name)
    }
  }
  return(NA_character_)
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

## structural equation for a root node
str_eq_root <- function(node) {

  # define data generation function string
  type <- get_distr_default(node$type)

  # get param string
  use_names <- !node$type %in% c("rnorm", "rbernoulli", "rcategorical",
                                 "rconstant")
  params <- get_param_str(node$params, use_names=use_names)

  # put together
  out <- paste0(node$name, " ~ ", type, "(", params, ")")

  return(out)
}

## structural equation for binomial child node
str_eq_binomial <- function(node) {

  if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ Bernoulli(logit())")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)

    if (!is.null(node$return_prob) && node$return_prob) {
      out <- paste0(node$name, " ~ logit(", node$intercept, " + ", beta_eq, ")")
    } else {
      out <- paste0(node$name, " ~ Bernoulli(logit(", node$intercept, " + ",
                    beta_eq, "))")
    }
  }
  return(out)
}

## structural equation for gaussian child node
str_eq_gaussian <- function(node) {
  if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ N()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ N(", node$intercept, " + ", beta_eq, ", ",
                  node$error, ")")
  }
  return(out)
}

## structural equation for poisson node
str_eq_poisson <- function(node) {
  if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ Poisson()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ Poisson(", node$intercept, " + ", beta_eq,
                  ")")
  }
  return(out)
}

## structural equation for negative binomial node
str_eq_negative_binomial <- function(node) {
  if (is.null(node$intercept)) {
    out <- paste0(node$name, " ~ NegBinomial()")
  } else {
    beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)
    out <- paste0(node$name, " ~ NegBinomial(", node$intercept, " + ", beta_eq,
                  " + log(", node$theta, "))")
  }
  return(out)
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

## structural equation for time-to-event / competing events node
str_eq_time_to_event <- function(node) {

  if (node$type=="time_to_event") {
    gen <- "Bernoulli"
  } else if (node$type=="competing_events") {
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
  beta_eq <- get_beta_plus_parents(betas=node$betas, parents=node$parents)

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

## structural equation for custom time-varying nodes
str_eq_td <- function(node) {
  args <- names(node)
  args <- args[!args %in% c("name", "type", "parents", "time_varying")]
  args <- paste0(args, collapse=", ")

  if ("sim_time" %in% names(formals(paste0("node_", node$type)))) {
    args <- paste0("t, ", args)
  }

  if (is.null(node$parents)) {
    out <- paste0(node$name, "(t) ~ node_", node$type, "(", args, ")")
  } else {
    parents <- paste0(paste0(node$parents, "(t)"), collapse=", ")
    out <- paste0(node$name, "(t) ~ node_", node$type, "(", parents, ", ",
                  args, ")")
  }
  return(out)
}

## structural equation for custom child nodes
str_eq_child <- function(node) {
  args <- names(node)
  args <- args[!args %in% c("name", "type", "parents", "time_varying")]
  args <- paste0(args, collapse=", ")

  parents <- paste0(node$parents, collapse=", ")

  out <- paste0(node$name, " ~ node_", node$type, "(", parents, ", ",
                args, ")")
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
    if (node$type %in% c("time_to_event", "competing_events")) {
      out <- str_eq_time_to_event(node)
    } else {
      out <- str_eq_td(node)
    }
  ## child nodes
  } else if (!is.null(node$parents)) {
    if (node$type %in% c("gaussian", "binomial", "conditional_prob",
                         "conditional_distr", "multinomial", "poisson",
                         "negative_binomial", "cox")) {
      str_eq_fun <- get(paste0("str_eq_", node$type))
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
