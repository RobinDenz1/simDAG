
## a node modeled using logistic regression
#' @export
node_binomial <- function(data, parents, betas, intercept) {
  prob <- intercept +
    rowSums(mapply("*", as.data.frame(data[, parents]), betas))
  prob <- 1/(1 + exp(-prob))
  out <- rbernoulli(n=nrow(data), p=prob)
  return(out)
}
