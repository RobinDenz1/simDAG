
## a node modeled using poisson-regression
#' @export
node_poisson <- function(data, parents, formula=NULL, betas, intercept,
                         var_corr=NULL, link="log") {

  # if formula includes random effects, use node_lmer() instead
  if (!is.null(formula) & !is.null(var_corr)) {
    out <- node_lmer(data=data, formula=formula, betas=betas,
                     intercept=intercept, var_corr=var_corr,
                     family=stats::poisson(link=link))
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

  mu <- calc_linpred(data=data, betas=betas, intercept=intercept)

  if (link=="log") {
    mu <- exp(mu)
  } else if (link=="sqrt") {
    mu <- mu^2
  }

  out <- vapply(mu, FUN=stats::rpois, FUN.VALUE=numeric(1), n=1)

  return(out)
}
