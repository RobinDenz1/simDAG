
## a node modeled using multinomial regression
# NOTE: betas must be a matrix with length(parents) columns and
#       n_classes rows
# TODO: this doesn't seem to work at all. Also, the intercepts are missing
#       needs some major reworks
#' @export
node_multinomial <- function(data, parents, betas, class_labels=NULL,
                             return_prob=FALSE) {
  # prep data
  mat <- as.matrix(data[, parents, with=FALSE])

  # generate scores and sample from those
  probs <- cbind(apply(betas, MARGIN=1, FUN=function(x, mat){mat %*% x},
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
