
## a node modeled using multinomial regression
# NOTE: betas must be a matrix with length(parents) columns and
#       n_classes rows
#       Output coefficients are always standardized to the coefficients of
#       the first category!
#' @export
node_multinomial <- function(data, parents, betas, intercepts,
                             labels=NULL, output="factor",
                             return_prob=FALSE) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  # prep data
  mat <- as.matrix(cbind(rep(1, nrow(data)), data[, parents, with=FALSE]))

  # add intercepts to betas
  betas <- t(cbind(intercepts, betas))

  # standardize to reference category 1
  betas <- betas - betas[, 1]

  # generate scores and convert them to probabilities
  probs <- (function(h) h / rowSums(h))(exp(mat %*% betas))

  # sample from those
  out <- rcategorical(n=nrow(data), probs=probs, labels=labels,
                      output=output)

  if (return_prob) {
    return(probs)
  } else {
    return(out)
  }
}
