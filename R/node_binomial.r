
## a node modeled using logistic regression
#' @export
node_binomial <- function(data, parents, betas, intercept,
                          return_prob=FALSE) {
  prob <- intercept +
    rowSums(mapply("*", as.data.frame(data[, parents, with=FALSE]), betas))
  prob <- 1/(1 + exp(-prob))
  out <- rbernoulli(n=nrow(data), p=prob)

  if (return_prob) {
    return(prob)
  } else {
    return(out)
  }
}
