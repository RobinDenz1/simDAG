
## a node modeled using poisson-regression
#' @export
node_poisson <- function(data, parents, betas, intercept) {
  eff <- rowSums(mapply("*", as.data.frame(data[, parents, with=FALSE]), betas))
  mu <- exp(intercept + eff)
  out <- vapply(mu, FUN=stats::rpois, FUN.VALUE=numeric(1), n=1)
  return(out)
}
