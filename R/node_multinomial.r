
## a node modeled using multinomial regression
# NOTE: betas must be a matrix with length(parents) columns and
#       n_classes rows
# TODO: this doesn't seem to work at all. Also, the intercepts are missing
#       needs some major reworks
#' @export
node_multinomial <- function(data, parents, betas, intercepts,
                             class_labels=NULL, return_prob=FALSE) {
  # prep data
  mat <- as.matrix(cbind(rep(1, nrow(data)), data[, parents, with=FALSE]))

  # add intercepts to betas
  betas <- cbind(intercepts, betas)

  # generate scores and sample from those
  probs <- cbind(apply(betas, MARGIN=1, FUN=function(x, mat){exp(mat %*% x)},
                       mat=mat))
  choice_mat <- t(apply(probs, 1, stats::rmultinom, n=1, size=1))
  out <- apply(choice_mat, 1, function(x) which(x==1))

  # turn into factor
  if (!is.null(class_labels)) {
    out <- factor(out, levels=class_labels)
  } else {
    out <- factor(out)
  }

  if (return_prob) {
    return(probs)
  } else {
    return(out)
  }
}
