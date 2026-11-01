
## generate data from a gamma regression
#' @importFrom data.table setDT
#' @export
node_gamma <- function(data, parents, formula=NULL, betas,
                       intercept, shape, link="log") {

  # check input arguments
  if (length(shape) != 1L || !is.numeric(shape) ||
      is.na(shape) || !is.finite(shape) || shape <= 0) {
    stop("'shape' must be a single, finite, positive numeric value.",
         call.=FALSE)
  }

  if (!is.character(link) || length(link) != 1L ||
      is.na(link) || !link %in% c("log", "identity", "inverse")) {
    stop("'link' must be either 'log', 'identity' or 'inverse'.",
         call.=FALSE)
  }

  if (!data.table::is.data.table(data)) {
    setDT(data)
  }

  # construct design matrix
  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)

    # remove intercept column, if present
    if ("(Intercept)" %in% colnames(data)) {
      data <- data[, colnames(data) != "(Intercept)", drop=FALSE]
    }

    # convert to data.frame for calc_linpred()
    data <- as.data.frame(data)

  } else {
    data <- data[, parents, with=FALSE]
  }

  # calculate linear predictor
  eta <- calc_linpred(data=data, betas=betas, intercept=intercept)

  # apply inverse link function to obtain conditional mean
  if (link == "log") {
    mu <- exp(eta)
  } else if (link == "identity") {
    mu <- eta
  } else if (link == "inverse") {
    mu <- 1 / eta
  }

  # check whether all non-missing means are valid
  invalid_mu <- !is.na(mu) & (!is.finite(mu) | mu <= 0)

  if (any(invalid_mu)) {
    stop("The conditional mean of the gamma distribution must be ",
         "finite and strictly positive for all observations.",
         call.=FALSE)
  }

  # generate outcome
  out <- stats::rgamma(n=length(mu), shape=shape, scale=mu / shape)

  return(out)
}
