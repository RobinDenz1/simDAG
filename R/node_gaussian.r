
## a node modeled using linear regression
#' @export
node_gaussian <- function(data, parents, formula=NULL, betas,
                          intercept, error, var_corr=NULL, link="identity") {

  # if formula includes random effects, use node_lmer() instead
  if (!is.null(formula) & !is.null(var_corr)) {
    out <- node_lmer(data=data, formula=formula, betas=betas,
                     intercept=intercept, var_corr=var_corr, error=error,
                     family=stats::gaussian(link=link))
    return(out)
  }

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  out <- calc_linpred(data=data, betas=betas, intercept=intercept)

  if (link=="log") {
    out <- exp(out)
  } else if (link=="inverse") {
    out <- 1 / out
  }

  out <- out + stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}

## calculates the linear predictor given data, betas and intercept
calc_linpred <- function(data, betas, intercept) {

  if (nrow(data)==1) {
    out <- intercept + sum(mapply("*", data, betas))
  } else {
    out <- intercept + rowSums(mapply("*", data, betas))
  }
  return(out)
}
