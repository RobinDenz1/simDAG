
## simulate survival time according to Bender et al. (2005)
sim_surv_time <- function(data, betas, dist, lambda, gamma=NULL, left=0) {

  eff <- calc_linpred(data=data, betas=betas, intercept=0)
  U <- stats::runif(nrow(data))

  # cumulative baseline hazard at left
  if (dist == "weibull") {
    H0_left <- lambda * left^gamma
  } else if (dist == "exponential") {
    H0_left <- lambda * left
  }

  # conditional survival inversion
  H0_t <- H0_left - log(1 - U) / exp(eff)

  # invert cumulative baseline hazard
  if (dist == "weibull") {
    surv_time <- (H0_t / lambda)^(1 / gamma)
  } else if (dist == "exponential") {
    surv_time <- H0_t / lambda
  }

  return(surv_time)
}

## a node modeled using cox-regression
#' @export
node_cox <- function(data, parents, formula=NULL, betas, surv_dist,
                     lambda, gamma, cens_dist=NULL, cens_args, name,
                     as_two_cols=TRUE, left=0, basehaz_grid=NULL,
                     extrapolate=FALSE, as_integer=FALSE, ...) {

  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  if (!is.null(formula)) {
    data <- stats::model.matrix(object=formula, data=data)
    data <- as.data.frame(data[, -1])
  } else {
    data <- as.data.frame(data[, parents, with=FALSE])
  }

  # generate survival times
  if (is.function(surv_dist)) {
    time <- rtrunc_surv(data=data, betas=betas, fbasehaz=surv_dist,
                        times=basehaz_grid, left=left, extrapolate=extrapolate,
                        ...)
  } else {
    time <- sim_surv_time(data=data, betas=betas, dist=surv_dist, lambda=lambda,
                          gamma=gamma, left=left)
  }

  if (!as_two_cols && is.null(cens_dist)) {
    if (as_integer) {
      out_data <- ceiling(time)
    } else {
      out_data <- time
    }
  } else {
    out_data <- add_censoring(times=time, cens_dist=cens_dist,
                              cens_args=cens_args, name=name)
    if (as_integer) {
      out_data[,1] <- ceiling(out_data[,1])
    }
  }

  return(out_data)
}

## add censoring to a survival time, if specified
#' @importFrom data.table fifelse
#' @importFrom data.table data.table
add_censoring <- function(times, cens_dist, cens_args, name) {

  if (!is.null(cens_dist)) {

    if (!is.function(cens_dist)) {
      cens_dist <- get(cens_dist)
    }

    cens_time <- do.call(cens_dist, c(n=length(times), cens_args))
    status <- fifelse(times < cens_time, 1, 0)
    times <- fifelse(times < cens_time, times, cens_time)
  } else {
    status <- 1
  }

  # put together in two columns
  out_data <- data.table(time=times, status=status)
  colnames(out_data) <- c(paste0(name, "_time"), paste0(name, "_status"))

  return(out_data)
}

## get cumulative baseline hazard  and its inverse as approximate functions
## using numerical integration and linear interpolation
get_lH0 <- function(f, times) {

  # check time grid
  if (!is.numeric(times) || length(times) < 1L ||
      anyNA(times) || any(!is.finite(times)) ||
      any(times <= 0) || any(diff(times) <= 0)) {
    stop("'times' must be a strictly increasing numeric vector",
         " containing only finite, positive values.", call.=FALSE)
  }

  # add time zero to the grid
  grid <- c(0, times)

  # evaluate baseline hazard at all grid points
  h0 <- f(grid)

  if (!is.numeric(h0) || length(h0) != length(grid) ||
      anyNA(h0) || any(!is.finite(h0)) || any(h0 <= 0)) {
    stop("'f' must return a vector of finite, strictly positive",
         " hazard values with the same length as its input.",
         call.=FALSE)
  }

  # calculate cumulative hazard using the trapezoidal rule
  increments <- diff(grid) * (h0[-length(h0)] + h0[-1]) / 2

  H0 <- c(0, cumsum(increments))

  # interpolate cumulative hazard
  H0_fun <- stats::approxfun(x=grid, y=H0, method="linear", rule=1)

  # interpolate inverse cumulative hazard
  invH0 <- stats::approxfun(x=H0, y=grid, method="linear", rule=2)

  return(list(H0=H0_fun, invH0=invH0))
}

## generate random (left-truncated) survival times from a Cox model given
## covariates, betas and a custom baseline hazard function
#' @importFrom data.table fifelse
rtrunc_surv <- function(data, betas, fbasehaz, times, left=0,
                        extrapolate=FALSE, lH0=NULL) {
  max_t <- max(times)

  if (is.null(lH0)) {
    lH0 <- get_lH0(f=fbasehaz, times=times)
  }

  # get cum. baseline hazard at left truncation times
  if (anyNA(left) || any(left > max_t)) {
    stop("Left truncation times must be smaller than max(basehaz_grid). Adjust",
         " the 'basehaz_grid' argument or the 'left' argument.", call.=FALSE)
  }
  H_l <- fifelse(left==0, 0, lH0$H0(left))

  # get exponentiated linear predictor
  eff <- exp(calc_linpred(data=data, betas=betas, intercept=0))

  # simulate cum. hazard for each individual
  target <- H_l + -log(stats::runif(nrow(data))) / eff

  if (!extrapolate && any(target > lH0$H0(max(times)))) {
    stop("Some randomly generated hazards go beyond the estimated cumulative",
         " baseline hazard as defined by the 'basehaz_grid' argument and will",
         " therefore be equal to max(basehaz_grid). Adjust the 'basehaz_grid'",
         " argument to include a longer time grid or set 'extrapolate=TRUE' to",
         " ignore this issue.", call.=FALSE)
  }

  # identify corresponding time from target
  times <- lH0$invH0(target)

  return(times)
}
