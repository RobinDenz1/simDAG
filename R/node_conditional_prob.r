
## generate dichotomous or categorical variable using conditional
## probabilities based on one or more categorical variables
#' @export
node_conditional_prob <- function(data, parents, probs, default_probs=NULL,
                                  default_val=NA, labels=NULL,
                                  coerce2factor=FALSE, check_inputs=TRUE) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (check_inputs) {
    check_inputs_node_conditional_probs(data=data,
                                        parents=parents,
                                        probs=probs,
                                        default_probs=default_probs,
                                        default_val=default_val)
  }

  # initialize variable
  out <- rep(default_val, nrow(data))

  # create interaction of parents if needed
  if (length(parents) > 1) {
    dep_var <- interaction(data[, parents, with=FALSE])
  } else {
    dep_var <- data[[parents]]
  }

  # levels of the dependent variable
  dep_levels <- as.character(unique(dep_var))

  # levels with defined probabilities in probs list
  dep_levels_def <- names(probs)

  # add default probs to missing levels in probs argument, if specified
  if (length(dep_levels_def) < length(dep_levels) && !is.null(default_probs)) {

    missing_levels <- dep_levels[!dep_levels %in% dep_levels_def]
    for (i in seq_len(length(missing_levels))) {
      probs[[missing_levels[i]]] <- default_probs
    }

    dep_levels_def <- names(probs)
  }

  # sample from corresponding probabilities
  # using bernoulli trials if there are just two classes and multinomial
  # trials with > 2 classes
  if (length(probs[[1]]) == 1) {
    for (i in seq_len(length(probs))) {
      out[dep_var==dep_levels_def[i]] <- rbernoulli(
        n=sum(dep_var==dep_levels_def[i]),
        p=probs[[dep_levels_def[i]]]
        )
    }
  } else {
    for (i in seq_len(length(probs))) {
      out[dep_var==dep_levels_def[i]] <- rcategorical(
        n=sum(dep_var==dep_levels_def[i]),
              probs=probs[[dep_levels_def[i]]],
              labels=NULL,
              output="numeric"
        )
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
