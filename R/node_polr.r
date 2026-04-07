
## generate data from an ordered logistic or probit regression
#' @export
node_polr <- function(data, parents, formula=NULL, betas,
                      cutpoints, link="logistic", labels=FALSE,
                      output="factor") {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  eta <- calc_linpred(data=data, betas=betas, intercept=0)

  # generate latent error depending on specified link function
  eps <- switch(link,
    logistic = stats::rlogis(nrow(data)),
    probit = stats::rnorm(nrow(data)),
    cloglog = -log(-log(stats::runif(nrow(data)))),
    loglog = log(-log(stats::runif(nrow(data)))),
    cauchit = stats::rcauchy(nrow(data))
  )

  # latent variable
  y_star <- eta + eps

  # ensure cutpoints sorted
  cutpoints <- sort(cutpoints)

  # assign categories
  out <- cut(y_star, breaks=c(-Inf, cutpoints, Inf), labels=labels,
             right=TRUE)

  if (output=="factor") {
    out <- factor(out, ordered=TRUE)
  } else if (output=="character") {
    out <- as.character(out)
  }

  return(out)
}
