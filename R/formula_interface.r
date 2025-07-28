
## check whether the extracted intercept is actually a valid intercept
check_intercept <- function(intercept) {
  if (length(intercept) > 1) {
    stop("Multiple intercepts or missing * found in 'formula'",
         ": ", paste0(intercept, collapse=", "), ".",
         " Please re-define the formula and re-run the function.",
         call.=FALSE)
  } else if (length(intercept) == 0) {
    stop("No intercept found in supplied 'formula'.", call.=FALSE)
  } else if (is.na(as.numeric(intercept))) {
    stop("Intercept supplied in 'formula' is not a number. ",
         "Supplied intercept: ", intercept, call.=FALSE)
  }
}

## check whether the variable * value format was always used
check_formlist <- function(formlist) {
  n_inlist <- vapply(formlist, length, FUN.VALUE=numeric(1))
  if (!all(n_inlist==2)) {
    stop("Missing variable name or coefficient in supplied 'formula'.",
         " The problem starts somewhere at: '", formlist[n_inlist!=2][[1]][1],
         "...'", call.=FALSE)
  }
}

## check whether betas in a formula are actually betas
check_betas <- function(betas) {
  if (anyNA(betas)) {
    stop("One or more of the supplied beta coefficients in 'formula'",
         " are not numbers.", call.=FALSE)
  }
}

## given different user input, parse to correct format for further processing
sanitize_formula <- function(formula) {

  if (is.null(formula)) {
    out <- NULL
  } else if (is_formula(formula)) {
    out <- paste0(str_trim(deparse(formula)), collapse="")
  } else if (is.vector(formula)) {
    out <- formula
  }

  # if it is not an enhanced formula, return it as formula
  if (!is.null(out) && !is_enhanced_formula(out)) {
    out <- formula
  }

  return(out)
}

## removes all whitespaces from start of string
str_trim <- function(string) {
  return(gsub("^\\s+", "", string))
}

## parses both numbers and functions calling numbers to their numeric value
str2numeric <- function(string) {
  out <- tryCatch({
    vapply(string,
           FUN=function(x){eval.parent(str2lang(x), n=6)},
           FUN.VALUE=numeric(1),
           USE.NAMES=FALSE)},
    error=function(e){
      stop("One or more of the supplied beta coefficients ",
           "in 'formula' are not numbers.", call.=FALSE)}
  )
  return(out)
}

## check if the node needs an intercept
node_needs_intercept <- function(node_type) {

  if (!is.function(node_type)) {
    fun_name <- paste0("node_", node_type)

    if (!exists(fun_name)) {
      fun_name <- node_type
    }
    node_type <- get(fun_name)
  }

  return("intercept" %in% names(formals(node_type)))
}

## parse custom formulas into betas, formula parts and intercept
# NOTE: expects formula to be a string, sanitized with sanitize_formula()
parse_formula <- function(formula, node_type) {

  if (!supports_mixed_terms(node_type) && has_mixed_terms(formula)) {
    stop("Random effects and random slopes are currently only supported in",
         " 'formula' for nodes of type 'gaussian', 'binomial', or",
         " 'poisson', not ", node_type, ".", call.=FALSE)
  }

  # clean up formula
  formstr <- gsub("~", "", formula, fixed=TRUE)
  formstr <- gsub(" ", "", formstr, fixed=TRUE)

  # processing of random effects and slopes
  if (has_mixed_terms(formstr)) {
    mixed_terms <- extract_mixed_terms(formstr)
    formstr <- str_replace_all(formstr, mixed_terms)
    formstr <- remove_mistaken_plus(formstr)
  } else {
    mixed_terms <- NULL
  }

  # split into additive parts
  formvec <- strsplit(formstr, "+", fixed=TRUE)[[1]]

  if (length(formvec) <= 1) {
    stop("A 'formula' cannot consist soley of random effects and/or random",
         " slopes. At least one fixed effect must also be supplied.",
         call.=FALSE)
  }

  # extract and check intercept
  has_star <- grepl("*", formvec, fixed=TRUE)
  needs_intercept <- node_needs_intercept(node_type)

  if (!needs_intercept) {
    intercept <- NULL
  } else {
    intercept <- formvec[!has_star]
    check_intercept(intercept)
  }

  # split rest further by variable / value pairs
  formvec <- formvec[has_star]
  formlist <- strsplit(formvec[grepl("*", formvec, fixed=TRUE)], "*",
                       fixed=TRUE)
  check_formlist(formlist)

  # extract in separate vectors
  n_parents <- length(formlist)
  formula_parts <- character(n_parents)
  betas <- character(n_parents)

  # determine order of formula
  if (!is_valid_number(formlist[[1]][[1]])) {
    ind1 <- 1
    ind2 <- 2
  } else {
    ind1 <- 2
    ind2 <- 1
  }

  for (i in seq_len(n_parents)) {
    formula_parts[i] <- formlist[[i]][ind1]
    betas[i] <- formlist[[i]][ind2]
  }

  # create betas
  betas <- str2numeric(betas)
  check_betas(betas)

  out <- list(formula_parts=formula_parts,
              mixed_terms=mixed_terms,
              betas=betas)

  if (needs_intercept) {
    out$intercept <- as.numeric(intercept)
  }

  return(out)
}

## given a special formula, update the arguments of a node
# NOTE: expects the formula to be input as string
args_from_formula <- function(args, formula, node_type) {
  form_parsed <- parse_formula(formula, node_type=node_type)

  args$parents <- form_parsed$formula_parts
  args$betas <- form_parsed$betas
  args$intercept <- form_parsed$intercept

  # adjust formula for mixed models, or remove it
  if (length(form_parsed$mixed_terms) > 0 &
      node_type %in% c("gaussian", "binomial", "poisson")) {

    args$mixed_terms <- form_parsed$mixed_terms
    args$formula <- get_formula_for_node_lmer(
      formula_parts=form_parsed$formula_parts,
      mixed_terms=form_parsed$mixed_terms
    )

    if (!"var_corr" %in% names(args)) {
      stop("'var_corr' must be specified when random effects or random",
           " slopes are included in 'formula'.", call.=FALSE)
    }

  } else {
    args$formula <- NULL
  }

  return(args)
}

## create a fitting dataset for special formula based nodes
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
data_for_formula <- function(data, args, networks=list()) {

  name <- NULL

  # extract variables mentioned in mixed effects parts, if included
  if (!is.null(args$mixed_terms)) {
    form_mixed <- stats::as.formula(paste0(
      "~ ", paste0(args$mixed_terms, collapse=" + ")
    ))
    mixed_parents <- all.vars(form_mixed)
    not_in_parents <- mixed_parents[!mixed_parents %in% args$parents]
    all_parents <- c(args$parents, not_in_parents)
  }

  # just return the relevant parts of the data.table if possible
  if (all(args$parents %in% colnames(data))) {
    if (is.null(args$mixed_terms)) {
      out <- data[, args$parents, with=FALSE]
    } else {
      out <- data[, all_parents, with=FALSE]
    }

    return(out)
  }

  # add all variables to model matrix
  form_dat <- paste0("~ `", paste0(colnames(data), collapse="` + `"), "`")

  # identify interactions
  form_int <- args$parents[grepl(":", args$parents, fixed=TRUE)]

  if (length(form_int) > 0) {

    # get all relevant interaction terms
    d_combs <- get_cat_col_levs(data)

    form_int_terms <- unique(
      vapply(X=strsplit(form_int, ":"),
             get_interaction_term_for_formula,
             FUN.VALUE=character(1),
             data=data, d_combs=d_combs)
    )

    # add them to the formula
    form_dat <- paste0(form_dat, " + ", paste0(form_int_terms, collapse=" + "))
  }

  # check for cubic terms
  form_cubic <- args$parents[grepl("I(", args$parents, fixed=TRUE)]

  if (length(form_cubic) > 0) {
    form_dat <- paste0(form_dat, " + ", paste0(form_cubic, collapse=" + "))
  }

  # check for network terms
  form_net <- get_net_terms(args$parents)

  if (length(form_net) > 0) {
    # add network terms to data
    d_net <- data.table(term=form_net,
                        expr=get_expr_from_net(form_net),
                        name=get_netname_from_net(form_net))

    # if there is just one network in the DAG, use it by default
    if (anyNA(d_net$name)) {
      if (length(networks)==1) {
        d_net[is.na(name), name := names(networks)]
      } else {
        stop("If more than one network() was added to the DAG object",
             " every net() call needs to specify which network should",
             " be used with the 'net' argument.", call.=FALSE)
      }
    }
    data <- add_network_info(data=data, d_net_terms=d_net, networks=networks)

    # add network terms to formula
    form_dat <- paste0(form_dat, " + `",
                       paste0(form_net, collapse="` + `"), "`")
  }

  # get full model matrix
  # (and print helpful error if column not found)
  mod_mat <- as.data.table(stats::model.matrix.lm(stats::as.formula(form_dat),
                                                  data=data,
                                                  na.action="na.pass"))

  if (length(form_net) > 0) {
    setnames(mod_mat, old=paste0("`", form_net, "`"), new=form_net)
  }

  mod_mat <- tryCatch({
    mod_mat[, args$parents, with=FALSE]},
    error=function(x){stop(x, "This error may occur when one of the terms in",
                           " a supplied formula does not match any variables",
                           " in the generated data.\n",
                           " Please check whether all terms in your supplied",
                           " formula occur in the data generated up to this",
                           " point.\n",
                           " The variables currently available in data are:\n",
                           paste0(colnames(mod_mat), collapse=", "),
                           call.=FALSE)}
  )

  # if it will be used for mixed models, append terms from those
  if (!is.null(args$mixed_terms)) {
    mod_mat <- as.data.frame(mod_mat)
    mod_mat <- cbind(mod_mat, data[, not_in_parents, with=FALSE])
  }

  return(mod_mat)
}

## creates a dataset that contains the possible levels of all
## categorical variables in a dataset
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
get_cat_col_levs <- function(data) {

  name <- categories <- NULL

  # identify categorical columns
  col_types <- vapply(data, class, FUN.VALUE=character(1))
  names(col_types) <- colnames(data)

  cat_cols <- colnames(data)[col_types %in% c("character", "logical", "factor")]

  # create all of their possible "names" in a dataset
  out <- vector(mode="list", length=length(cat_cols))
  for (i in seq_len(length(cat_cols))) {

    # extract possible category names
    if (col_types[cat_cols[i]]=="factor") {
      levs <- levels(data[[cat_cols[i]]])
    } else {
      levs <- unique(data[[cat_cols[i]]])
    }

    out[[i]] <- data.table(name=cat_cols[i],
                           categories=as.character(levs))
  }
  d_combs <- rbindlist(out)
  d_combs[, levs := paste0(name, categories)]

  return(d_combs)
}

## function to generate interaction terms to be used in the formula
## that generates the full model matrix
get_interaction_term_for_formula <- function(parts, data, d_combs) {

  cnames <- colnames(data)

  cols <- character(length(parts))
  for (i in seq_len(length(parts))) {
    # check if in column names or in potential categorical values
    guess1 <- cnames[parts[i]==cnames]
    guess2 <- d_combs$name[d_combs$levs==parts[i]]

    if (length(guess1)==1) {
      cols[i] <- guess1
    } else if (length(guess2)==1) {
      cols[i] <- guess2
    } else {
      stop("The variable '", parts[i], "' named in the interaction '",
           paste0(parts, collapse=":"), "' is neither an existing column",
           " nor a columname plus a valid level of a categorical variable.",
           call.=FALSE)
    }
  }

  term <- paste0(cols, collapse=" * ")
  return(term)
}

## check if two objects are the same
## this is essentially equivalent to the new version of isTRUE(all.equal())
is_same_object <- function(fun1, fun2) {
  out <- all.equal(fun1, fun2)
  return(is.logical(out) && length(out)==1 && !is.na(out) && out)
}

## check whether a string is a number or a function called on a number
## or in a special case, eval() called on a variable
is_valid_number <- function(string) {
  out <-
    grepl("^(-?\\d+(\\.\\d+)?)$", string) ||
    grepl("^([a-zA-Z0-9_\\.]+\\(-?\\d+(\\.\\d+)?\\))$", string) ||
    grepl("^(eval\\([a-zA-Z0-9_\\.]*\\))$", string)
  return(out)
}

## check whether a string represents an enhanced formula as implemented
## in this package
is_enhanced_formula <- function(string) {

  # remove white space
  string <- gsub(" ", "", string, fixed=TRUE)

  # checks in this order:
  # 1. contains *NUMBER
  # 2. contains *FUNCTION(NUMBER)
  # 3. contains eval(VARIABLE)
  # 4. contains NUMBER*
  # 5. contains | (for random effects / slopes)
  out <-
    grepl("\\*-?\\d+(\\.\\d+)?", string) ||
    grepl("\\*[a-zA-Z0-9_\\.]+\\(-?\\d+(\\.\\d+)?\\)", string) ||
    grepl("eval\\([a-zA-Z0-9_\\.]*\\)", string) ||
    grepl("\\d+\\)?\\s*\\*", string) ||
    grepl("|", string, fixed=TRUE)
  return(out)
}

## get parents out of a special formula object
parents_from_formula <- function(formula, node_type) {

  parsed <- parse_formula(formula, node_type=node_type)

  formula_parts1 <- parsed$formula_parts
  raw_formula <- stats::as.formula(
    paste0("~ ", paste0(c(parsed$formula_parts, parsed$mixed_terms),
                        collapse=" + "))
  )

  return(all.vars(raw_formula))
}
