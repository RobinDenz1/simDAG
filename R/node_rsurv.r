
## simulate data using functions from rsurv package
node_rsurv <- function(data, parents, betas, baseline, dist, package, u, type,
                       cens_dist, cens_args, name, as_two_cols=TRUE,
                       left=0, right=Inf, ...) {

  requireNamespace("rsurv", quietly=TRUE)

  # prepare inputs
  formula <- formula_from_parents(parents)
  data <- as.data.frame(data)

  # pass to relevant rsurv function
  rsurv_fun <- utils::getFromNamespace(paste0("r", type), "rsurv")

  if (utils::packageVersion("rsurv") >= "0.0.3") {
    times <- rsurv_fun(data=data, formula=formula, beta=betas,
                       baseline=baseline, dist=dist, package=package,
                       u=u, lwr=left, upr=right, ...)
  } else {
    times <- rsurv_fun(data=data, formula=formula, beta=betas,
                       baseline=baseline, dist=dist, package=package,
                       u=u, ...)
    if (any(left > 0 | right < Inf)) {
      warning("The arguments 'left' and 'right' require 'rsurv'",
              " package version 0.0.3 or higher. Please update the",
              " package and re-run this function.", call.=FALSE)
    }
  }

  # apply censoring if specified
  if (!as_two_cols && is.null(cens_dist)) {
    out_data <- as.numeric(times)
  } else {
    out_data <- add_censoring(times=as.numeric(times), cens_dist=cens_dist,
                              cens_args=cens_args, name=name)
  }

  return(out_data)
}

## create a formula object which includes all parents as additive terms
formula_from_parents <- function(parents) {
  stats::as.formula(paste0("~ ", paste(parents, collapse=" + ")))
}

## simulate data from accelerated failure time model
#' @export
node_aftreg <- function(data, parents, betas, baseline, dist=NULL,
                        package=NULL, u=stats::runif(nrow(data)),
                        cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                        left=0, right=Inf, ...) {

  out <- node_rsurv(data=data, parents=parents, betas=betas,
                    baseline=baseline, dist=dist, package=package, u=u,
                    cens_dist=cens_dist, cens_args=cens_args, name=name,
                    as_two_cols=as_two_cols, type="aftreg", left=left,
                    right=right, ...)
  return(out)
}

## simulate data from accelerated hazard model
#' @export
node_ahreg <- function(data, parents, betas, baseline, dist=NULL,
                       package=NULL, u=stats::runif(nrow(data)),
                       cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                       left=0, right=Inf, ...) {

  out <- node_rsurv(data=data, parents=parents, betas=betas,
                    baseline=baseline, dist=dist, package=package, u=u,
                    cens_dist=cens_dist, cens_args=cens_args, name=name,
                    as_two_cols=as_two_cols, type="ahreg", left=left,
                    right=right, ...)
  return(out)
}

## simulate data from a extended hazard model
#' @export
node_ehreg <- function(data, parents, betas, phi, baseline, dist=NULL,
                       package=NULL, u=stats::runif(nrow(data)),
                       cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                       left=0, right=Inf, ...) {

  out <- node_rsurv(data=data, parents=parents, betas=betas, phi=phi,
                    baseline=baseline, dist=dist, package=package, u=u,
                    cens_dist=cens_dist, cens_args=cens_args, name=name,
                    as_two_cols=as_two_cols, type="ehreg", left=left,
                    right=right, ...)
  return(out)
}

## simulate data from proportional odds model
#' @export
node_poreg <- function(data, parents, betas, baseline, dist=NULL,
                       package=NULL, u=stats::runif(nrow(data)),
                       cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                       left=0, right=Inf, ...) {

  out <- node_rsurv(data=data, parents=parents, betas=betas,
                    baseline=baseline, dist=dist, package=package, u=u,
                    cens_dist=cens_dist, cens_args=cens_args, name=name,
                    as_two_cols=as_two_cols, type="poreg", left=left,
                    right=right, ...)
  return(out)
}

## simulate data from a Yang & Prentice model
#' @export
node_ypreg <- function(data, parents, betas, phi, baseline, dist=NULL,
                       package=NULL, u=stats::runif(nrow(data)),
                       cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                       left=0, right=Inf, ...) {

  out <- node_rsurv(data=data, parents=parents, betas=betas, phi=phi,
                    baseline=baseline, dist=dist, package=package, u=u,
                    cens_dist=cens_dist, cens_args=cens_args, name=name,
                    as_two_cols=as_two_cols, type="ypreg", left=left,
                    right=right, ...)
  return(out)
}
