
## get information for linear node from data
gen_node_gaussian <- function(name, parents, data, return_model=FALSE, ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="gaussian")
  args <- c(args, ...)

  # fit model
  model <- do.call(stats::glm, args)

  # extract coef, intercept, sigma
  out <- list(name=name,
              parents=parents,
              type="gaussian",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]),
              error=stats::sd(model$residuals))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## get information for binomial node from data
gen_node_binomial <- function(name, parents, data, return_model=FALSE, ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="binomial")
  args <- c(args, ...)

  # fit model
  model <- do.call(stats::glm, args)

  # extract coef, intercept
  out <- list(name=name,
              parents=parents,
              type="binomial",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## get information for poisson node from data
gen_node_poisson <- function(name, parents, data, return_model=FALSE, ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="poisson")
  args <- c(args, ...)

  # fit model
  model <- do.call(stats::glm, args)

  # extract coef, intercept
  out <- list(name=name,
              parents=parents,
              type="poisson",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## gaussian root node from data
gen_node_rnorm <- function(data, name, na.rm) {
  out <- list(name=name,
              type="rnorm",
              params=list(mean=mean(data[[name]], na.rm=na.rm),
                          sd=stats::sd(data[[name]], na.rm=na.rm)))
  return(out)
}

## binomial root node from data
gen_node_rbernoulli <- function(data, name, na.rm) {
  out <- list(name=name,
              type="rbernoulli",
              params=list(p=mean(data[[name]], na.rm=na.rm)))
  return(out)
}

## multinomial root node from data
gen_node_rcategorical <- function(data, name, na.rm) {
  tab <- prop.table(table(data[[name]]))
  out <- list(name=name,
              type="rcategorical",
              params=list(labels=names(tab), probs=tab))
}

## given minimal information on node type and the causal structure,
## create lists for the root_nodes and child_nodes from observed data
## by fitting appropriate models
#' @export
dag_from_data <- function(dag, data, return_models=FALSE,
                          na.rm=FALSE) {
  # initialize new dag
  new_dag <- empty_dag()
  models <- vector(mode="list", length=length(dag$child_nodes))

  # fill new_dag with new root nodes
  for (i in seq_len(length(dag$root_nodes))) {

    # call associated root function
    root_fun <- get(paste0("gen_node_", dag$root_nodes[[i]]$type))

    new_dag$root_nodes[[length(new_dag$root_nodes)+1]] <-
      root_fun(data=data, name=dag$root_nodes[[i]]$name, na.rm=na.rm)
  }

  # fill new_dag with new child nodes
  for (i in seq_len(length(dag$child_nodes))) {

    # get new node using model_node_ function
    model_fun <- get(paste0("gen_node_", dag$child_nodes[[i]]$type))

    args <- list(data=data,
                 name=dag$child_nodes[[i]]$name,
                 parents=dag$child_nodes[[i]]$parents)

    new_node <- do.call(model_fun, args)

    # add model to new list if specified
    if (return_models) {
      models[[i]] <- new_node$model
    }
    new_node$model <- NULL

    # add node to new dag
    new_dag$child_nodes[[length(new_dag$child_nodes)+1]] <- new_node
  }

  out <- list(dag=new_dag,
              models=models)

  return(out)
}
