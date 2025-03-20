
## a node modeled using logistic regression
#' @export
node_binomial <- function(data, parents, formula=NULL, betas, intercept,
                          return_prob=FALSE, output="logical", labels=NULL,
                          var_corr=NULL) {

  # if formula includes random effects, use node_lmer() instead
  if (!is.null(formula) & !is.null(var_corr)) {
    out <- node_lmer(data=data, formula=formula, betas=betas,
                     intercept=intercept, var_corr=var_corr,
                     family="binomial")
  } else {

    if (!data.table::is.data.table(data)) {
      data.table::setDT(data)
    }

    if (!is.null(formula)) {
      data <- stats::model.matrix(object=formula, data=data)
      data <- as.data.frame(data[, -1])
    } else {
      data <- as.data.frame(data[, parents, with=FALSE])
    }

    prob <- intercept +
      rowSums(mapply("*", data, betas))
    prob <- 1/(1 + exp(-prob))
    out <- rbernoulli(n=nrow(data), p=prob)
  }

  if (output %in% c("character", "factor") && !is.null(labels)) {
    out[out] <- labels[1]
    out[out!=labels[1]] <- labels[2]
  }

  if (output=="logical") {
    out <- as.logical(out)
  } else if (output=="factor" & is.null(labels)) {
    out <- factor(out)
  } else if (output=="factor") {
    out <- factor(out, levels=labels)
  } else if (output=="numeric") {
    out <- as.numeric(out)
  }

  if (return_prob & is.null(var_corr)) {
    return(prob)
  } else {
    return(out)
  }
}
