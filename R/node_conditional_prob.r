
## generate dichotomous or categorical variable using conditional
## probabilities based on one or more categorical variables
#' @export
node_conditional_prob <- function(data, parents, probs, labels=NULL,
                                  coerce2factor=FALSE, check_inputs=TRUE) {

  if (check_inputs) {
    check_inputs_node_conditional_probs(data=data,
                                        parents=parents,
                                        probs=probs)
  }

  # initialize variable
  if (length(probs[[1]]) == 1) {
    out <- rep(FALSE, nrow(data))
  } else {
    out <- rep(NA_integer_, nrow(data))
  }

  # levels of the dependent variable
  dep_levels <- names(probs)

  # create interaction of parents if needed
  if (length(parents) > 1) {
    dep_var <- interaction(data[, parents, with=FALSE])
  } else {
    dep_var <- data[[parents]]
  }

  # sample from corresponding probabilities
  # using bernoulli trials if there are just two classes and multinomial
  # trials with > 2 classes
  if (length(probs[[1]]) == 1) {
    for (i in seq_len(length(probs))) {
      out[dep_var==dep_levels[i]] <- rbernoulli(n=sum(dep_var==dep_levels[i]),
                                                p=probs[[dep_levels[i]]])
    }
  } else {
    for (i in seq_len(length(probs))) {
      out[dep_var==dep_levels[i]] <- rcategorical(n=sum(dep_var==dep_levels[i]),
                                                  probs=probs[[dep_levels[i]]],
                                                  labels=NULL,
                                                  coerce2factor=FALSE)
    }
  }

  # set labels / coerce2factor
  if (coerce2factor & is.null(labels)) {
    out <- factor(out)
  } else if (coerce2factor) {
    out <- factor(out, labels=labels)
  }

  return(out)
}
