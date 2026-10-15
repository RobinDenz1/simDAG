\name{calibrate_dag}
\alias{calibrate_dag}

\title{
Calibrate a partially specified \code{DAG} to match target values
}
\description{
This function may be used to calibrate certain parameters in a partially specified \code{DAG}, given some global targets. For example, users may be interested in fixing marginal effects (such as the average causal treatment effect), rather than fixing the conditional treatment effects as represented by beta coefficients. See details.
}
\usage{
calibrate_dag(lazy_dag, targets, params, n_sim_run=100000,
              n_sim_eval=1000000, n_reps_eval=1,
              seed=stats::runif(1, 0, 100),
              optimizer="auto", optim_method=NULL,
              optim_control=list(maxit=200), objective_fun=NULL,
              ...)
}
\arguments{
  \item{lazy_dag}{
An unevaluated, partially specified \code{lazy_DAG} object created using the \code{\link{lazy_dag}} function. See \code{?lazy_dag} for more information on lazy DAGs and the associated \code{param()} syntax. See \code{?node} for general information on how to define DAGs.
  }
  \item{targets}{
A \code{list} containing one \code{list} per desired target. These sub-lists need to contain at least two named elements: \code{target}, a numeric value specifying the exact desired value of the target and \code{eval_fun}, a function to evaluate the target value given simulated data with \code{n_sim_run} individuals or the current version of the \code{DAG} (if an additional \code{use_dag=TRUE} element is included). The \code{eval_fun} should only have one argument, which is the simulated data or DAG, and return exactly one numeric value relating to the target. Additionally, users may include a \code{scale} element, which should be a single numeric value specifying a scaling factor included during optimization. If not supplied, it defaults to 1. May be useful when targets are on widely different scales. This argument may specify any number of targets. See details, examples and the associated vignette.
  }
  \item{params}{
A named \code{list} specifying the free parameters of the partially specified DAG. The names must correspond to the parameter placeholders introduced by \code{param()} in the \code{lazy_dag}. Each parameter specification must contain a finite numeric start value and may optionally contain the following elements:

\strong{\code{range}}: A numeric vector of length two specifying the lower and upper bounds for the parameter. If omitted, the parameter is treated as unbounded. Bounds are used by optimization methods that support them. For a single parameter and a single target, a finite range is required for automatic root finding using \code{\link[stats]{uniroot}}.

\strong{\code{scale}}: A positive numeric scalar specifying the parameter scale used by the numerical optimizer. If omitted, the scale is determined automatically from the parameter range or starting value.

Parameter specifications may be supplied as named lists or named numeric vectors when all supplied elements are scalar. For example, \code{list(par1 = c(start = 1))} and \code{list(par1 = list(start = 1))} are both valid. If the DAG contains only one free parameter, its specification may be supplied directly, without an additional named list, e.g. \code{list(start = 0, range = c(-5, 5))}. In this case, the parameter name is inferred from the corresponding \code{param()} placeholder.
  }
  \item{n_sim_run}{
A single positive number, specifying the sample size that should be used when evaluating the \code{targets} during optimization. Higher numbers lead to more stable, reliable and precise estimates, but may take much longer to run. It often makes sense to use a smaller number first, to adjust the starting values with the (perhaps non-converged) results and to then re-run \code{calibrate_dag()} with a much higher value for more precise results.
  }
  \item{n_sim_eval}{
A single positive number, specifying the sample size used when evaluating the \code{targets} after optimization has finished. This is primarily done to estimate how well the optimization worked. To obtain reliable and precise results, it should either be a fairly large number with a single simulation repetition (e.g. \code{n_reps_eval=1}), or a moderate number with a large number of repetitions. The latter has the advantage that sampling distributions can be plotted and confidence intervals can be estimated, but it comes at the cost of higher computation time.
  }
  \item{n_reps_eval}{
A single positive number, specifying how many simulation repetitions should be performed when evaluating the final parameter values. See \code{n_sim_eval}.
  }
  \item{seed}{
A random number generator seed. If not \code{NULL}, this seed is used on every single evaluation of the underlying changing \code{DAG}, eliminating some random sampling variation. The only exception is the final evaluation using a sample of size \code{n_sim_eval}, which instead uses \code{seed + 1} (if possible), to make the evaluation independent of the optimization. Set to \code{NULL} to use an independent dataset during each optimization iteration instead.
  }
  \item{optimizer}{
A single character string, specifying which optimizer should be used. Allowed values are: \code{"uniroot"} to use \code{\link[stats]{uniroot}}, \code{"optim"} to use \code{\link[stats]{optim}}, \code{"optim2"} to use the \code{\link[calibrar]{optim2}} function from the \pkg{calibrar} package, \code{"optimh"} to use the \code{\link[calibrar]{optimh}} function from the \pkg{calibrar} package and \code{"auto"} (default), which is equal to \code{"uniroot"} whenever possible (e.g. whenever there is one target, one free parameter and defined bounds on that parameter) and equal to \code{"optim"} otherwise.
  }
  \item{optim_method}{
A single character string, specifying which \code{method} should be used in the underlying \code{optimizer} function call. Ignored if \code{"uniroot"} is used as an \code{optimizer}.
  }
  \item{optim_control}{
This argument is directly passed to the \code{control} argument of the respective \code{optimiter} function. Ignored if \code{"uniroot"} is used as an \code{optimizer}.
  }
  \item{objective_fun}{
A custom function that should be used in the underlying \code{optimizer} function call to evaluate how close the current value of the \code{targets} are to the desired results. Any function that takes a numeric vector of (scaled) differences between the desired and estimated value and returns a scalar number that can be minimized may be used. Ignored if \code{"uniroot"} is used as an \code{optimizer}.
  }
  \item{...}{
Further arguments passed to the \code{optimizer} and the \code{\link{sim_n_datasets}} function, which is used internally to generate one dataset (using \code{n_repeats = 1}). All named arguments are first passed through the \code{optimizer}.
  }
}
\details{

We recommend consulting the associated vignette for a more detailed explanation, including multiple worked examples, available at \code{vignette(topic="v_calibrate_dag", package="simDAG")}.

\strong{\emph{What it does}}:

This function tries to help find suitable conditional parameters, when the goal is to fix marginal ones. Instead of defining all conditional parameters directly, users may specify arbitrary (potentially marginal) global parameters instead and to leave the conditional parameters partially unspecified. It then tries to \emph{calibrate} the unspecified parameters, so that the global targets are met. The resulting \code{DAG} can then be used to simulate the data with the desired global target properties using \code{\link{sim_from_dag}}, \code{\link{sim_discrete_time}} or \code{\link{sim_discrete_event}}.

\strong{\emph{How it works}}:

Instead of defining a fully specified \code{DAG}, users should define only a partially specified \code{DAG}, leaving one or more parameters unspecified. This should be done using the \code{\link{lazy_dag}} function (see \code{?lazy_dag} for more information). Afterwards, users can define an arbitrary amount of targets through custom \code{targets} functions. These functions should simply calculate the current value of the desired property. For example, if the data should have a 50\% prevalence in the binary variable \code{Y}, a suitable target function would simply be \code{function(data) {mean(data$Y)}} with an associated \code{target = 0.5}. Additionally, users should give rough ranges in which the unspecified parameters may lie.

Internally, root finding (if 1 target and 1 unspecified parameter, with defined \code{range}s) or numerical optimization (all other cases) is then used to find suitable values for the unspecified parameters that satisfy the target properties defined by the user. In a nutshell, the function simply simulates a large dataset from a given parameter combination, checks the resulting target properties and tries again. This idea, although applied in a less general setting, is also described by Austin and Stafford (2008) and Austin (2023).

To inspect the found parameter values and how well those produce the desired targets, users may use the associated \code{summary()} method (see examples). If \code{n_reps_eval > 1}, the associated \code{\link{plot.calibrated_DAG}} method may also be used. To simulate data directly using the resulting parameters, users may use the \code{dag} object returned in the output.

\strong{\emph{Currently supported features}}:

Users may use custom functions to define the target properties, so there are very few limits to what can be specified. Additionally, any parameter in a valid expression that defines a \code{DAG} may be left unspecified. Furthermore, there is no limit put on either the number of target properties (denoted \eqn{m}) or the number of unspecified parameters (denoted \eqn{p}), although some care needs to be exercised. If \eqn{p > m}, there may be multiple different solutions, which is usually not problematic, but should be noted. If \eqn{m > p}, there likely aren't any solutions, unless some \code{targets} are highly correlated. DAGs of any kind (using discrete-time varying covariates, continuous-time varying covariates, network dependencies, ...) may be used here.

\strong{\emph{Limitations}}:

There is unfortunately no guarantee that a solution for any given specification exist. Even worse, there is no guarantee that if a solution exists, that this function will find it. It is, however, still much better than manually searching for a solution. Furthermore, users should keep in mind that even if a solution exists and the numeric optimization is able to converge to it, that the results are only approximate. In most cases, this should be completely fine for Monte-Carlo simulation studies. However, in some edge cases, if exact simulation specifications of marginal parameters are required, this may not be sufficient.

\strong{\emph{Convergence}}:

Convergence may be tricky with complicated \code{DAG}s, many \code{params} or many \code{targets} (or both). Ultimately, the same techniques that work for any numerical optimization may be used to obtain convergence, e.g. tuning the \code{optim_control} arguments, re-running the function with starting values from a previous run etc. The only thing specific to this function that has a large effect on convergence is the \code{n_sim_run} argument. Better convergence may be obtained with larger values, at the price of a longer computation time.

}
\value{
Returns a \code{calibrated_DAG} object. This object is basically just a \code{list}, containing the following objects:

\itemize{
  \item{\code{inputs}: A list containing all user inputs.}
  \item{\code{dag}: A fully specified \code{DAG}, using the best sets of parameter values found to satisfy the user targets. This \code{DAG} may be directly used for simulation purposes.}
  \item{\code{est_params}: The final parameter values after optimization.}
  \item{\code{target_eval}: A final estimate of the \code{targets}, evaluated on the final \code{DAG}. If the function call was successful, these should be very close to the desired target parameters. If \code{n_reps_eval > 1}, these correspond to the average over all simulation repetitions.}
  \item{\code{d_target_eval}: If \code{n_reps_eval > 1} was used, this object includes three \code{data.table}s, including the estimated targets on each simulation run, the differences to the desired values and the scaled differences.}
  \item{\code{optim_obj}: If \code{\link[stats]{optim}}, \code{\link[calibrar]{optim2}} or \code{\link[calibrar]{optimh}} was used, the resulting object from that function call.}
  \item{\code{uniroot}: If \code{\link[stats]{uniroot}} was used, the resulting object from that function call.}
}

}
\author{
Robin Denz
}
\references{
Denz, Robin and Nina Timmesfeld (2026). "Simulating Complex Cross-Sectional and Longitudinal Data using the simDAG R Package". Journal of Statistical Software 116 (2), doi: 10.18637/jss.v116.i02.

Austin, Peter C. 2023. "The Iterative Bisection Procedure: A Useful Tool for Determining Parameter Values in Data-Generating Processes in Monte Carlo Simulations."" BMC Medical Research Methodology 23 (45): 1–10.

Austin, Peter C., and James Stafford. 2008. "The Performance of Two Data-Generation Processes for Data with Specified Marginal Treatment Odds Ratios."" Communications in Statistics: Simulation and Computation 37 (6): 1039–51.
}
\examples{
library(simDAG)
library(data.table)

# Example 1: Calibrate a single parameter to match a target prevalence.
# A finite parameter range allows automatic root finding.

ldag <- lazy_dag(
  empty_dag() +
    node("X", type="rnorm") +
    node("A", type="binomial", formula= ~ param(intercept_A) + X*0.5)
)

targets <- list(
  prevalence_A = list(
    target = 0.4,
    eval_fun = function(data) mean(data$A)
  )
)

# For a single parameter, its specification can be supplied directly.
params <- list(
  start = 0,
  range = c(-5, 5)
)

fit <- calibrate_dag(
  lazy_dag = ldag,
  targets = targets,
  params = params,
  n_sim_run = 2000,
  n_sim_eval = 5000,
  seed = 123
)
summary(fit)

# use final DAG to simulate data
data <- sim_from_dag(fit$dag, n_sim=10)
head(data)

# Example 2: Jointly calibrate treatment and outcome prevalence.
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
  n_sim_eval = 5000,
  seed = 123,
  optim_control = list(
    maxit = 200,
    trace = 0,
    ndeps = c(0.05, 0.05),
    factr = 100
  )
)
summary(fit)

# use final DAG to simulate data
data <- sim_from_dag(fit$dag, n_sim=10)
head(data)
}
