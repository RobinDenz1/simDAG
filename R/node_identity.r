
## a node that simply evaluates an expression on variables already present
## in the data, such as ~ A + B + 4
#' @export
node_identity <- function(data, parents, formula) {

  # parse formula to string, remove leading ~
  form_str <- paste0(str_trim(deparse(formula)), collapse="")
  form_str <- substr(form_str, start=2, stop=nchar(form_str))

  # evaluate expression on data
  out <- with(data, eval(str2lang(form_str)))

  return(out)
}

## check if supplied node type is a node of type "identity"
is_identity_node <- function(node) {
  (length(node)==1 && is.character(node) && node=="identity") ||
  (is.function(node) && extract_function_name(node)=="node_identity")
}
