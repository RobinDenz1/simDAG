
## a node modeled using logistic regression
#' @export
node_binomial <- function(data, parents, formula=NULL, betas, intercept,
                          return_prob=FALSE, coerce2factor=FALSE,
                          coerce2numeric=FALSE, labels=NULL) {

  if (!is.null(formula)) {
    data <- stats::model.frame(formula=formula, data=data)
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  prob <- intercept +
    rowSums(mapply("*", data, betas))
  prob <- 1/(1 + exp(-prob))
  out <- rbernoulli(n=nrow(data), p=prob)

  if (!is.null(labels)) {
    out[out] <- labels[1]
    out[out!=labels[1]] <- labels[2]
  }

  if (coerce2factor & is.null(labels)) {
    out <- factor(out)
  } else if (coerce2factor) {
    out <- factor(out, levels=labels)
  }

  if (is.null(labels) & !coerce2factor & coerce2numeric) {
    out <- as.numeric(out)
  }

  if (return_prob) {
    return(prob)
  } else {
    return(out)
  }
}
