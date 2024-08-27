
## check whether the extracted intercept is actually a valid intercept
check_intercept <- function(intercept) {
  if (length(intercept) > 1) {
    stop("Multiple intercepts or missing * found in 'formula'",
         ": ", paste0(intercept, collapse=", "), ".",
         " Please re-define the formula and re-run the function.")
  } else if (length(intercept) == 0) {
    stop("No intercept found in supplied 'formula'.")
  } else if (is.na(as.numeric(intercept))) {
    stop("Intercept supplied in 'formula' is not a number. ",
         "Supplied intercept: ", intercept)
  }
}

## check whether the variable * value format was always used
check_formlist <- function(formlist) {
  n_inlist <- vapply(formlist, length, FUN.VALUE=numeric(1))
  if (!all(n_inlist==2)) {
    stop("Missing variable name or coefficient in supplied 'formula'.",
         " The problem starts somewhere at: '", formlist[n_inlist!=2][[1]][1],
         "...'")
  }
}

## check whether betas in a formula are actually betas
check_betas <- function(betas) {
  if (anyNA(betas)) {
    stop("One or more of the supplied beta coefficients in 'formula'",
         " are not numbers.")
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

  out <- tryCatch({vapply(string, FUN=function(x){eval(str2lang(x))},
                          FUN.VALUE=numeric(1), USE.NAMES=FALSE)},
                  error=function(e){
                    stop("One or more of the supplied beta coefficients ",
                         "in 'formula' are not numbers.")})
  return(out)
}

## parse custom formulas into betas, formula parts and intercept
# NOTE: expects formula to be a string, sanitized with sanitize_formula()
parse_formula <- function(formula, node_type) {

  # clean up formula
  formstr <- gsub("~", "", formula, fixed=TRUE)
  formstr <- gsub(" ", "", formstr, fixed=TRUE)

  # split into additive parts
  formvec <- strsplit(formstr, "+", fixed=TRUE)[[1]]

  # extract and check intercept
  ind <- grepl("*", formvec, fixed=TRUE)

  is_cox_node <- (is.function(node_type) &&
                  is_same_object(node_type, node_cox)) ||
    (!is.function(node_type) && node_type=="cox")

  if (is_cox_node) {
    intercept <- NULL
  } else {
    intercept <- formvec[!ind]
    check_intercept(intercept)
  }

  # split rest further by variable / value pairs
  formvec <- formvec[ind]
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
              betas=betas)

  if (!is_cox_node) {
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
  args$formula <- NULL

  return(args)
}

## create a fitting dataset for special formula based nodes
#' @importFrom data.table as.data.table
data_for_formula <- function(data, args) {

  # just return the relevant parts of the data.table if possible
  if (all(args$parents %in% colnames(data))) {
    return(data[, args$parents, with=FALSE])
  }

  # add all variables to model matrix
  form_dat <- paste0("~ ", paste0(colnames(data), collapse=" + "))

  # identify interactions
  form_int <- args$parents[grepl(":", args$parents, fixed=TRUE)]

  if (length(form_int) > 0) {

    # get all relevant interaction terms
    d_combs <- get_cat_col_levs(data)

    form_int_terms <- strsplit(form_int, ":") |>
      vapply(get_interaction_term_for_formula, FUN.VALUE=character(1),
             data=data, d_combs=d_combs) |>
      unique()

    # add them to the formula
    form_dat <- paste0(form_dat, " + ", paste0(form_int_terms, collapse=" + "))
  }

  # check for cubic terms
  form_cubic <- args$parents[grepl("I(", args$parents, fixed=TRUE)]

  if (length(form_cubic) > 0) {
    form_dat <- paste0(form_dat, " + ", paste0(form_cubic, collapse=" + "))
  }

  # get full model matrix
  # (and print helpful error if column not found)
  mod_mat <- as.data.table(stats::model.matrix.lm(stats::as.formula(form_dat),
                                                  data=data,
                                                  na.action="na.pass"))

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
                           call.=FALSE)})
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
is_valid_number <- function(string) {
  return(grepl("^(\\d+(\\.\\d+)?|[a-zA-Z]+\\(\\d+(\\.\\d+)?\\))$", string))
}

## check whether a string represents an enhanced formula as implemented
## in this package
is_enhanced_formula <- function(string) {
  out <- grepl("\\*\\s*(-?\\d+|[a-zA-Z]+\\s*\\(-?\\d+\\))", string) ||
    grepl("\\d+\\)?\\s*\\*", string)
  return(out)
}

## get parents out of a special formula object
parents_from_formula <- function(formula, node_type) {

  formula_parts <- parse_formula(formula, node_type=node_type)$formula_parts
  raw_formula <- stats::as.formula(paste0("~ ", paste0(formula_parts,
                                   collapse=" + ")))

  return(all.vars(raw_formula))
}
