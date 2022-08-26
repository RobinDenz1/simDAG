
## a node modeled using multinomial regression
# NOTE: betas must be a matrix with length(parents) columns and
#       n_classes rows
# TODO: This works, but its unclear how to incorporate this into the
#       other code if the node has children
#' @export
node_multinomial <- function(data, parents, betas, class_labels=NULL) {
  # prep data
  mat <- as.matrix(data[, parents, with=FALSE])

  # generate scores and sample from those
  probs <- cbind(apply(betas, MARGIN=1, FUN=function(x, mat){mat %*% x},
                       mat=mat))
  choice_mat <- t(apply(probs, 1, stats::rmultinom, n = 1, size = 1))
  out <- apply(choice_mat, 1, function(x) which(x==1))

  # turn into factor
  if (!is.null(class_labels)) {
    out <- factor(out, levels=class_labels)
  } else {
    out <- factor(out)
  }
  return(out)
}
