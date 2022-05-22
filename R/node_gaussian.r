
## a node modeled using linear regression
#' @export
node_gaussian <- function(data, parents, betas, intercept, error) {
  out <- intercept +
    rowSums(mapply("*", as.data.frame(data[, parents]), betas)) +
    stats::rnorm(n=nrow(data), mean=0, sd=error)
  return(out)
}
