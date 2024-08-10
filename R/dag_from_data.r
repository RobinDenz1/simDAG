
## general function for glm nodes
gen_glm_node <- function(name, parents, data, return_model, na.rm, type) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family=type)

  # fit model
  model <- do.call(stats::glm, args)

  # extract coef, intercept
  out <- list(name=name,
              type_str=type,
              type_fun=get(paste0("node_", type)),
              parents=parents,
              time_varying=FALSE,
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]))

  if (type=="gaussian") {
    out$error <- stats::sd(model$residuals)
  }

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## get information for linear node from data
gen_node_gaussian <- function(name, parents, data, return_model, na.rm) {

  out <- gen_glm_node(name=name, parents=parents, data=data,
                      return_model=return_model, type="gaussian")
  return(out)
}

## get information for binomial node from data
gen_node_binomial <- function(name, parents, data, return_model, na.rm) {

  out <- gen_glm_node(name=name, parents=parents, data=data,
                      return_model=return_model, type="binomial")
  return(out)
}

## get information for poisson node from data
gen_node_poisson <- function(name, parents, data, return_model, na.rm) {

  out <- gen_glm_node(name=name, parents=parents, data=data,
                      return_model=return_model, type="poisson")
  return(out)
}

## get information for negative binomial node from data
gen_node_negative_binomial <- function(name, parents, data, return_model,
                                       na.rm) {
  requireNamespace("MASS")

  # fit negative binomial regression
  form <- paste0(name, " ~ ", paste0(parents, collapse=" + "))
  model <- MASS::glm.nb(stats::as.formula(form), data=data,
                        na.action=ifelse(na.rm, "na.omit", "na.fail"))

  # extract needed information
  out <- list(name=name,
              type_str="negative_binomial",
              type_fun=node_negative_binomial,
              parents=parents,
              time_varying=FALSE,
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]),
              theta=model$theta)

  if (return_model) {
    out$model <- model
  }

  return(out)
}

## get information for conditional probability node from data
#' @importFrom data.table :=
gen_node_conditional_prob <- function(data, name, parents, return_model,
                                      na.rm) {

  prob <- ..interact_parents.. <- NULL

  data$..interact_parents.. <- interaction(data[, parents, with=FALSE])

  # estimate probabilities
  data[, prob := mean(eval(parse(text=name)), na.rm=na.rm),
       by=..interact_parents..]
  data <- unique(data[, c("..interact_parents..", "prob")])

  # coerce to list
  probs <- as.list(data$prob)
  names(probs) <- data$..interact_parents..

  out <- list(name=name,
              type_str="conditional_prob",
              type_fun=node_conditional_prob,
              parents=parents,
              time_varying=FALSE,
              probs=probs)

  return(out)
}

## gaussian root node from data
gen_node_rnorm <- function(data, name, na.rm) {
  out <- list(name=name,
              type_str="rnorm",
              type_fun=stats::rnorm,
              parents=NULL,
              time_varying=FALSE,
              params=list(mean=mean(data[[name]], na.rm=na.rm),
                          sd=stats::sd(data[[name]], na.rm=na.rm)))
  return(out)
}

## binomial root node from data
gen_node_rbernoulli <- function(data, name, na.rm) {
  out <- list(name=name,
              type_str="rbernoulli",
              type_fun=rbernoulli,
              parents=NULL,
              time_varying=FALSE,
              params=list(p=mean(data[[name]], na.rm=na.rm)))
  return(out)
}

## multinomial root node from data
gen_node_rcategorical <- function(data, name, na.rm) {
  tab <- prop.table(table(data[[name]]))
  out <- list(name=name,
              type_str="rcategorical",
              type_fun=rcategorical,
              parents=NULL,
              time_varying=FALSE,
              params=list(labels=names(tab), probs=as.vector(tab)))
  return(out)
}

## given minimal information on node type and the causal structure,
## create lists for the root_nodes and child_nodes from observed data
## by fitting appropriate models
#' @export
dag_from_data <- function(dag, data, return_models=FALSE, na.rm=FALSE) {

  check_inputs_dag_from_data(dag=dag, data=data, return_models=return_models,
                             na.rm=na.rm)

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  # initialize new dag
  new_dag <- empty_dag()
  models <- vector(mode="list", length=length(dag$child_nodes))

  # fill new_dag with new root nodes
  for (i in seq_len(length(dag$root_nodes))) {

    fun_name <- paste0("gen_node_", dag$root_nodes[[i]]$type_str)

    if (!exists(fun_name, mode="function")) {
      stop("The function '", fun_name, "' neccessary to create the node",
           " object for node '", dag$root_nodes[[i]]$name, "' is not",
           " defined. Users need to write their own functions for ",
           " unsupported node types. See details.")
    }

    # call associated root function
    root_fun <- get(fun_name)

    new_node <- root_fun(data=data, name=dag$root_nodes[[i]]$name, na.rm=na.rm)
    class(new_node) <- "DAG.node"

    new_dag$root_nodes[[length(new_dag$root_nodes)+1]] <- new_node

  }

  # fill new_dag with new child nodes
  for (i in seq_len(length(dag$child_nodes))) {

    fun_name <- paste0("gen_node_", dag$child_nodes[[i]]$type_str)

    if (!exists(fun_name, mode="function")) {
      stop("The function '", fun_name, "' neccessary to create the node",
           " object for node '", dag$child_nodes[[i]]$name, "' is not",
           " defined. Users need to write their own functions for ",
           " unsupported node types. See details.")
    }

    # get new node using model_node_ function
    model_fun <- get(fun_name)

    args <- list(data=data,
                 name=dag$child_nodes[[i]]$name,
                 parents=dag$child_nodes[[i]]$parents,
                 return_model=return_models,
                 na.rm=na.rm)

    new_node <- do.call(model_fun, args)

    # add model to new list if specified
    if (return_models) {
      models[[i]] <- new_node$model
    }
    new_node$model <- NULL

    class(new_node) <- "DAG.node"

    # add node to new dag
    new_dag$child_nodes[[length(new_dag$child_nodes)+1]] <- new_node
  }

  out <- list(dag=new_dag,
              models=models)

  return(out)
}
