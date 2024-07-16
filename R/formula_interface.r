
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
    out <- paste0(deparse(formula), collapse="")
  } else if (is.vector(formula)) {
    out <- formula
  }

  if (!is.null(out) && !grepl("\\*\\s*\\d+", out)) {
    out <- formula
  }

  return(out)
}

## parse custom formulas into betas, formula parts and intercept
# NOTE: expects formula to be a string, sanitized with sanitize_formula()
parse_formula <- function(formula) {

  # clean up formula
  formstr <- gsub("~", "", formula, fixed=TRUE)
  formstr <- gsub(" ", "", formstr, fixed=TRUE)

  # split into additive parts
  formvec <- strsplit(formstr, "+", fixed=TRUE)[[1]]

  # extract and check intercept
  ind <- grepl("*", formvec, fixed=TRUE)
  intercept <- formvec[!ind]
  check_intercept(intercept)

  # split rest further by variable / value pairs
  formvec <- formvec[ind]
  formlist <- strsplit(formvec[grepl("*", formvec, fixed=TRUE)], "*",
                       fixed=TRUE)
  check_formlist(formlist)

  # extract in separate vectors
  n_parents <- length(formlist)
  formula_parts <- character(n_parents)
  betas <- character(n_parents)

  for (i in seq_len(n_parents)) {
    formula_parts[i] <- formlist[[i]][1]
    betas[i] <- formlist[[i]][2]
  }

  # create betas
  betas <- as.numeric(betas)
  check_betas(betas)

  out <- list(formula_parts=formula_parts,
              betas=betas,
              intercept=as.numeric(intercept))
  return(out)
}

## given a special formula, update the arguments of a node
# NOTE: expects the formula to be input as string
args_from_formula <- function(args, formula) {
  form_parsed <- parse_formula(formula)

  args$parents <- form_parsed$formula_parts
  args$betas <- form_parsed$betas
  args$intercept <- form_parsed$intercept
  args$formula <- NULL

  return(args)
}

## create a fitting dataset for special formula based nodes
#' @importFrom data.table as.data.table
data_for_formula <- function(data, args) {

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
  mod_mat <- as.data.table(stats::model.matrix(stats::as.formula(form_dat),
                                               data=data))
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
