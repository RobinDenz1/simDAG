
## generate data from a beta regression
#' @importFrom data.table is.data.table
#' @importFrom data.table setDT
#' @export
node_beta <- function(data, parents, formula=NULL, betas,
                      intercept, precision, link="logit") {

  # check input arguments
  if (length(precision) != 1L || !is.numeric(precision) ||
      is.na(precision) || !is.finite(precision) || precision <= 0) {
    stop("'precision' must be a single, finite, positive numeric value.",
         call.=FALSE)
  }

  allowed_links <- c("logit", "probit", "cloglog", "loglog", "cauchit")
  if (!is.character(link) || length(link) != 1L ||
      is.na(link) || !link %in% allowed_links) {
    stop("'link' must be one of: ",
         paste0("'", allowed_links, "'", collapse=", "), ".",
         call.=FALSE)
  }

  if (!is.data.table(data)) {
    setDT(data)
  }

  # construct design matrix
  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)

    # remove intercept column, if present
    if ("(Intercept)" %in% colnames(data)) {
      data <- data[, colnames(data) != "(Intercept)", drop=FALSE]
    }

    # calc_linpred() expects a data.frame-like object
    data <- as.data.frame(data)
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  # calculate linear predictor
  if (ncol(data) == 0L) {
    eta <- rep(intercept, nrow(data))
  } else {
    eta <- calc_linpred(data=data, betas=betas, intercept=intercept)
  }

  # apply inverse link function to obtain conditional mean
  if (link == "logit") {
    mu <- stats::plogis(eta)
  } else if (link == "probit") {
    mu <- stats::pnorm(eta)
  } else if (link == "cloglog") {
    mu <- -expm1(-exp(eta))
  } else if (link == "loglog") {
    mu <- exp(-exp(-eta))
  } else if (link == "cauchit") {
    mu <- stats::pcauchy(eta)
  }

  # check whether all non-missing means are valid
  invalid_mu <- is.nan(mu) | (!is.na(mu) &
    (!is.finite(mu) | mu <= 0 | mu >= 1))

  if (any(invalid_mu)) {
    stop("The conditional mean of the beta distribution must be ",
         "finite and strictly between zero and one for all observations.",
         call.=FALSE)
  }

  # generate outcome
  out <- stats::rbeta(n=length(mu), shape1=mu * precision,
                      shape2=(1 - mu) * precision)

  return(out)
}
