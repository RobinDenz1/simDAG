
## generate survival times from an Aalen additive hazards model
## (with time-constant coefficients and time-constant baseline hazard)
#' @importFrom data.table as.data.table
#' @export
node_aalen <- function(data, parents, formula=NULL, betas, intercept,
                       cens_dist=NULL, cens_args, name, as_two_cols=TRUE,
                       left=0) {

  data <- as.data.table(data)
  data <- data[, parents, with=FALSE]

  hazard <- calc_linpred(data=data, betas=betas, intercept=intercept)

  if (min(hazard) < 0) {
    stop("Hazards < 0 encountered, meaning the model cannot be simulated",
         " from as specified. Please change the coefficients or other",
         " parts of the data generation process to ensure positive hazards.",
         call.=FALSE)
  }

  u <- stats::runif(nrow(data))
  time <- left - log(u) / hazard

  if (!as_two_cols && is.null(cens_dist)) {
    out_data <- time
  } else {
    out_data <- add_censoring(times=time, cens_dist=cens_dist,
                              cens_args=cens_args, name=name)
  }

  return(out_data)
}
