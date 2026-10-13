# Calibrate a partially specified `DAG` to match target values

This function may be used to calibrate certain parameters in a partially
specified `DAG`, given some global targets. For example, users may be
interested in fixing marginal effects (such as the average causal
treatment effect), rather than fixing the conditional treatment effects
as represented by beta coefficients. See details.

## Usage

``` r
calibrate_dag(lazy_dag, targets, params, n_sim_run=100000,
              n_sim_eval=1000000, seed=stats::runif(1, 0, 100),
              optimizer="auto", optim_method=NULL,
              optim_control=list(maxit=200), objective_fun=NULL,
              ...)
```

## Arguments

- lazy_dag:

  An unevaluated, partially specified `lazy_DAG` object created using
  the
  [`lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
  function. See
  [`?lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
  for more information on lazy DAGs and the associated `param()` syntax.
  See [`?node`](https://robindenz1.github.io/simDAG/reference/node.md)
  for general information on how to define DAGs.

- targets:

  A `list` containing one `list` per desired target. These sub-lists
  need to contain at least two named elements: `target`, a numeric value
  specifying the exact desired value of the target and `eval_fun`, a
  function to evaluate the target value given simulated data with
  `n_sim_run` individuals or the current version of the `DAG` (if an
  additional `use_dag=TRUE` element is included). The `eval_fun` should
  only have one argument, which is the simulated data or DAG, and return
  exactly one numeric value relating to the target. Additionally, users
  may include a `scale` element, which should be a single numeric value
  specifying a scaling factor included during optimization. If not
  supplied, it defaults to 1. May be useful when targets are on widely
  different scales. This argument may specify any number of targets. See
  details, examples and the associated vignette.

- params:

  A named `list` specifying the free parameters of the partially
  specified DAG. The names must correspond to the parameter placeholders
  introduced by `param()` in the `lazy_dag`. Each parameter
  specification must contain a finite numeric start value and may
  optionally contain the following elements:

  **`range`**: A numeric vector of length two specifying the lower and
  upper bounds for the parameter. If omitted, the parameter is treated
  as unbounded. Bounds are used by optimization methods that support
  them. For a single parameter and a single target, a finite range is
  required for automatic root finding using
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html).

  **`scale`**: A positive numeric scalar specifying the parameter scale
  used by the numerical optimizer. If omitted, the scale is determined
  automatically from the parameter range or starting value.

  Parameter specifications may be supplied as named lists or named
  numeric vectors when all supplied elements are scalar. For example,
  `list(par1 = c(start = 1))` and `list(par1 = list(start = 1))` are
  both valid. If the DAG contains only one free parameter, its
  specification may be supplied directly, without an additional named
  list, e.g. `list(start = 0, range = c(-5, 5))`. In this case, the
  parameter name is inferred from the corresponding `param()`
  placeholder.

- n_sim_run:

  A single positive number, specifying the sample size that should be
  used when evaluating the `targets` during optimization. Higher numbers
  lead to more stable, reliable and precise estimates, but may take much
  longer to run. It often makes sense to use a smaller number first, to
  adjust the starting values with the (perhaps non-converged) results
  and to then re-run `calibrate_dag()` with a much higher value for more
  precise results.

- n_sim_eval:

  A single positive number, specifying the sample size used when
  evaluating the `targets` after optimization has finished. This is
  primarily done to estimate how well the optimization worked and should
  thus always be a fairly large number for precise results.

- seed:

  A random number generator seed. If not `NULL`, this seed is used on
  every single evaluation of the underlying changing `DAG`, eliminating
  some random sampling variation. The only exception is the final
  evaluation using a sample of size `n_sim_eval`, which instead uses
  `seed + 1` (if possible), to make the evaluation independent of the
  optimization.

- optimizer:

  A single character string, specifying which optimizer should be used.
  Allowed values are: `"uniroot"` to use
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html), `"optim"` to use
  [`optim`](https://rdrr.io/r/stats/optim.html), `"optim2"` to use the
  [`optim2`](https://roliveros-ramos.github.io/calibrar/reference/optim2.html)
  function from the calibrar package, `"optimh"` to use the
  [`optimh`](https://roliveros-ramos.github.io/calibrar/reference/optimh.html)
  function from the calibrar package and `"auto"` (default), which is
  equal to `"uniroot"` whenever possible (e.g. whenever there is one
  target, one free parameter and defined bounds on that parameter) and
  equal to `"optim"` otherwise.

- optim_method:

  A single character string, specifying which `method` should be used in
  the underlying `optimizer` function call. Ignored if `"uniroot"` is
  used as an `optimizer`.

- optim_control:

  This argument is directly passed to the `control` argument of the
  respective `optimiter` function. Ignored if `"uniroot"` is used as an
  `optimizer`.

- objective_fun:

  A custom function that should be used in the underlying `optimizer`
  function call to evaluate how close the current value of the `targets`
  are to the desired results. Any function that takes a numeric vector
  of (scaled) differences between the desired and estimated value and
  returns a scalar number that can be minimized may be used. Ignored if
  `"uniroot"` is used as an `optimizer`.

- ...:

  Further arguments passed to the `optimizer` and the
  [`sim_n_datasets`](https://robindenz1.github.io/simDAG/reference/sim_n_datasets.md)
  function, which is used internally to generate one dataset (using
  `n_repeats = 1`). All named arguments are first passed through the
  `optimizer`.

## Details

We recommend consulting the associated vignette for a more detailed
explanation, including multiple worked examples, available at
[`vignette(topic="v_calibrate_dag", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_calibrate_dag.md).

***What it does***:

This function tries to help find suitable conditional parameters, when
the goal is to fix marginal ones. Instead of defining all conditional
parameters directly, users may specify arbitrary (potentially marginal)
global parameters instead and to leave the conditional parameters
partially unspecified. This function then tries to *calibrate* the
unspecified parameters, so that the global targets are met. The
resulting `DAG` can then be used to simulate the data with the desired
global target properties using
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md).

***How it works***:

Instead of defining a fully specified `DAG`, users should define only a
partially specified `DAG`, leaving one or more parameters unspecified.
This should be done using the
[`lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
function (see
[`?lazy_dag`](https://robindenz1.github.io/simDAG/reference/lazy_dag.md)
for more information). Afterwards, users can define an arbitrary amount
of targets through custom `targets` functions. These functions should
simply calculate the current value of the desired property. For example,
if the data should have a 50% prevalence in the binary variable `Y`, a
suitable target function would simply be `function(data) {mean(data$Y)}`
with an associated `target = 0.5`. Additionally, users should give rough
ranges in which the unspecified parameters may lie.

Internally, root finding (if 1 target and 1 unspecified parameter) or
numerical optimization (all other cases) is then used to find suitable
values for the unspecified parameters that satisfy the target properties
defined by the user. In a nutshell, the function simply simulates a
large dataset from a given parameter combination, checks the resulting
target properties and tries again. This idea, although applied in a less
general setting, is also described by Austin and Stafford (2008) and
Austin (2023).

***Currently supported features***:

Users may use custom functions to define the target properties, so there
are very few limits to what can be specified. Additionally, any
parameter in a valid expression that defines a `DAG` may be left
unspecified. Furthermore, there is no limit put on either the number of
target properties (denoted \\t\\) or the number of unspecified
parameters (denoted \\p\\), although some care needs to be exercised. If
\\p \> t\\, there may be multiple different solutions, which is usually
not problematic, but should be noted. DAGs of any kind (using
discrete-time varying covariates, continuous-time varying covariates,
network dependencies, ...) may be used here.

***Limitations***:

There is unfortunately no guarantee that a solution for any given
specification exist. Even worse, there is no guarantee that if a
solution exists, that this function will find it. It is, however, still
much better than manually searching for a solution. Furthermore, users
should keep in mind that even if a solution exists and the numeric
optimization is able to converge to it, that the results are only
approximate. In most cases, this should be completely fine for
Monte-Carlo simulation studies. However, if exact simulation
specifications of marginal parameters are required, this may not be
sufficient.

***Convergence***:

Convergence may be tricky with complicated `DAG`s, many `params` or many
`targets` (or both). Ultimately, the same techniques that work for any
numerical optimization may be used to obtain convergence, e.g. tuning
the `optim_control` arguments, re-running the function with starting
values from a previous run etc. The only thing specific to this function
that has a large effect on convergence is the `n_sim_run` argument.
Better convergence may be obtained with larger values, at the price of a
longer computation time.

## Value

Returns a `calibrated_DAG` object. This object is basically just a
`list`, containing the following objects:

- `inputs`: A list containing all user inputs.

- `dag`: A fully specified `DAG`, using the best sets of parameter
  values found to satisfy the user targets. This `DAG` may be directly
  used for simulation purposes.

- `est_params`: The final parameter values after optimization.

- `target_eval`: A final estimate of the `targets`, evaluated on the
  final `DAG`. If the function call was successful, these should be very
  close to the desired target parameters.

- `optim_obj`: If [`optim`](https://rdrr.io/r/stats/optim.html),
  [`optim2`](https://roliveros-ramos.github.io/calibrar/reference/optim2.html)
  or
  [`optimh`](https://roliveros-ramos.github.io/calibrar/reference/optimh.html)
  was used, the resulting object from that function call.

- `uniroot`: If [`uniroot`](https://rdrr.io/r/stats/uniroot.html) was
  used, the resulting object from that function call.

## Author

Robin Denz

## References

Denz, Robin and Nina Timmesfeld (2026). "Simulating Complex
Cross-Sectional and Longitudinal Data using the simDAG R Package".
Journal of Statistical Software 116 (2), doi: 10.18637/jss.v116.i02.

Austin, Peter C. 2023. "The Iterative Bisection Procedure: A Useful Tool
for Determining Parameter Values in Data-Generating Processes in Monte
Carlo Simulations."" BMC Medical Research Methodology 23 (45): 1–10.

Austin, Peter C., and James Stafford. 2008. "The Performance of Two
Data-Generation Processes for Data with Specified Marginal Treatment
Odds Ratios."" Communications in Statistics: Simulation and Computation
37 (6): 1039–51.

## Examples

``` r
library(simDAG)
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%

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
#> ────────────────────────────────────────────────────────────
#> Calibrated DAG
#> ────────────────────────────────────────────────────────────
#> Parameters (1)
#>    parameter estimate start lower upper parscale
#>  intercept_A  -0.3881     0    -5     5       NA
#> 
#> Targets (1)
#>        target desired achieved  error scaled_error
#>  prevalence_A     0.4   0.4186 0.0186       0.0186
#> 
#> Optimization
#>   Method:       uniroot
#>   Convergence:  successful
#>   Root residual: 0
#>   Iterations:   9
#>   Simulations:  2,000 optimization; 5,000 final evaluation
#> ────────────────────────────────────────────────────────────

# use final DAG to simulate data
data <- sim_from_dag(fit$dag, n_sim=10)
head(data)
#>              X      A
#>          <num> <lgcl>
#> 1: -0.39968354  FALSE
#> 2: -1.08779926   TRUE
#> 3: -1.09722672  FALSE
#> 4: -1.15883079   TRUE
#> 5: -0.03121757  FALSE
#> 6: -0.40738889   TRUE

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
#> ────────────────────────────────────────────────────────────
#> Calibrated DAG
#> ────────────────────────────────────────────────────────────
#> Parameters (2)
#>    parameter estimate start lower upper parscale
#>  intercept_A  -0.3828     0  -Inf   Inf        1
#>  intercept_Y  -1.1368     0  -Inf   Inf        1
#> 
#> Targets (2)
#>        target desired achieved  error scaled_error
#>  prevalence_A     0.4   0.4204 0.0204       0.0204
#>  prevalence_Y     0.3   0.3118 0.0118       0.0118
#> 
#> Optimization
#>   Optimizer:    optim()
#>   Method:       L-BFGS-B
#>   Convergence:  unsuccessful (code 52)
#>   Objective:    5e-07
#>   Evaluations:  function 47, gradient 47
#>   Message:      ERROR: ABNORMAL_TERMINATION_IN_LNSRCH
#>   Simulations:  2,000 optimization; 5,000 final evaluation
#> ────────────────────────────────────────────────────────────

# use final DAG to simulate data
data <- sim_from_dag(fit$dag, n_sim=10)
head(data)
#>             X      A      Y
#>         <num> <lgcl> <lgcl>
#> 1: -1.9845624  FALSE  FALSE
#> 2:  0.5216279   TRUE   TRUE
#> 3: -0.5202400  FALSE  FALSE
#> 4:  0.5802165  FALSE  FALSE
#> 5: -0.4531919  FALSE  FALSE
#> 6: -1.4561329   TRUE  FALSE
```
