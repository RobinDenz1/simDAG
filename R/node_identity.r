
## a node that simply evaluates an expression on variables already present
## in the data, such as ~ A + B + 4
#' @export
node_identity <- function(data, parents, formula, kind="expr",
                          betas, intercept, var_names=NULL,
                          name=NULL, dag=NULL) {

  if (kind=="expr") {
    # parse formula to string, remove leading ~
    form_str <- paste0(str_trim(deparse(formula)), collapse="")
    form_str <- substr(form_str, start=2, stop=nchar(form_str))

    # evaluate expression on data
    out <- with(data, eval(str2lang(form_str)))
  } else if (kind=="linpred") {
    out <- node_identity.linpred(data=data, formula=formula, dag=dag)
  } else if (kind=="data") {
    out <- node_identity.data(data=data, formula=formula, dag=dag,
                              var_names=var_names, name=name)
  }

  return(out)
}

## returns the linear predictor given an enhanced formula
node_identity.linpred <- function(data, formula, dag) {

  # parse formula
  form_str <- sanitize_formula(formula)
  form_parsed <- parse_formula(formula=form_str, node_type="gaussian")
  form_parsed$parents <- form_parsed$formula_parts

  # get data
  data <- data_for_formula(data=data, args=form_parsed, networks=dag$networks)

  # calculate linear predictor
  out <- form_parsed$intercept + rowSums(mapply("*", data, form_parsed$betas))

  return(out)
}

## returns only the data generated using data_from_formula()
node_identity.data <- function(data, formula, dag, var_names, name) {

  if (is_formula(formula)) {
    formstr <- paste0(str_trim(deparse(formula)), collapse="")
  } else if (is.vector(formula)) {
    formstr <- formula
  }

  if (has_mixed_terms(formstr)) {
    stop("Mixed model terms are currently not supported in nodes of",
         " type='identity'.", call.=FALSE)
  }

  formstr <- gsub("~", "", formstr, fixed=TRUE)
  formstr <- gsub(" ", "", formstr, fixed=TRUE)

  if (grepl("+", formstr, fixed=TRUE)) {
    formstr <- strsplit(formstr, "+", fixed=TRUE)[[1]]
  } else {
    var_names <- name
  }

  args <- list(parents=formstr, mixed_terms=NULL)
  out <- data_for_formula(data=data, args=args, networks=dag$networks)

  if (!is.null(var_names)) {
    setnames(out, old=colnames(out), new=var_names)
  }

  return(out)
}

## check if supplied node type is a node of type "identity"
is_identity_node <- function(node) {
  (length(node)==1 && is.character(node) && node=="identity") ||
  (is.function(node) && extract_function_name(node)=="node_identity")
}
