\name{plot.calibrated_DAG}
\alias{plot.calibrated_DAG}

\title{
Plot a \code{calibrated_DAG} object
}
\description{
This function may be used to visually inspect how well a \code{\link{calibrate_dag}} function call was able to approximate the desired targets. If fitted with \code{n_reps_eval > 1}, it plots the distribution of the targets estimated on the \code{n_reps_eval} evaluation datasets using the determined parameters. Alternatively plots the bias directly.
}
\usage{
\method{plot}{calibrated_DAG}(x, type="errors", xlab=NULL,
     ylab="auto", line_at_0=TRUE, ...)
}
\arguments{
  \item{x}{
A \code{calibrated_DAG} object created using the \code{\link{calibrate_dag}} function.
  }
  \item{type}{
A single character string, specifying what should be plotted. Should be either \code{"targets"} (to plot the raw target estimates), \code{"scaled_targets"} (to plot the target estimates after applying the user-specified scaling factors, if applicable), \code{"errors"} (to plot the difference between the estimated targets and the desired targets, the default) or \code{"scaled_errors"} (same as errors, but after applying the user-specified scaling factors, if applicable).
  }
  \item{xlab}{
A single character string specifying the name of the x-axis or \code{NULL} (default) to not use a name.
  }
  \item{ylab}{
A single character string specifying the name of the y-axis or \code{NULL} to not use a name. If set to \code{"auto"} (default), the label will be determined automatically based on the supplied \code{type}.
  }
  \item{line_at_0}{
Either \code{TRUE} or \code{FALSE}, specifying whether to plot a dashed line at 0 whenever \code{type = "errors"} or \code{type = "scaled_errors"} is used, ignored otherwise.
  }
  \item{...}{
Further arguments passed to the \code{\link[ggplot2]{geom_boxplot}} function, allowing users to customize their appearance further.
  }
}
\details{
The function internally relies on the \pkg{ggplot2} package, so any further \code{ggplot2} syntax may be used to customize the plot. Its main use is to visually assess whether the observed discrepancy between the estimated and desired targets is due to sampling error or a genuine difference.
}
\author{
Robin Denz
}
\value{
Returns a standard \code{ggplot2} object.
}
\examples{
# Jointly calibrate treatment and outcome prevalence.
# The DAG contains two free parameters and two targets.
# Parameter ranges are optional.

ldag <- lazy_dag(
  empty_dag() +
    node("X", type="rnorm") +
    node("A", type="binomial",
         formula= ~ param(intercept_A) + X * 0.5) +
    node("Y", type="binomial",
         formula= ~ param(intercept_Y) + A * 0.7 + X * 0.4)
)

targets <- list(
  prevalence_A = list(
    target = 0.4,
    eval_fun = function(data) mean(data$A)
  ),
  prevalence_Y = list(
    target = 0.3,
    eval_fun = function(data) mean(data$Y)
  )
)

params <- list(
  intercept_A = list(start = 0),
  intercept_Y = list(start = 0)
)

fit <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 2000,
  n_sim_eval = 1000,
  n_reps_eval = 10,
  seed = 123,
  optim_control = list(
    maxit = 200,
    trace = 0,
    ndeps = c(0.05, 0.05),
    factr = 100
  )
)
summary(fit)

# plot the targets directly
plot(fit, type="targets")

# plot the errors instead
plot(fit, type="errors")
}
