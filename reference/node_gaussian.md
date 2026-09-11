# Generate Data from a (Mixed) Linear Regression Model

Data from the parents is used to generate the node using linear
regression by predicting the covariate specific mean and sampling from a
normal distribution with that mean and a specified standard deviation.
Allows inclusion of arbitrary random effects and slopes.

## Usage

``` r
node_gaussian(data, parents, formula=NULL, betas, intercept, error,
              var_corr=NULL, link="identity")
```

## Arguments

- data:

  A `data.table` (or something that can be coerced to a `data.table`)
  containing all columns specified by `parents`.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has. If non-linear combinations or interaction
  effects should be included, the user may specify the `formula`
  argument instead.

- formula:

  An optional `formula` object to describe how the node should be
  generated or `NULL` (default). If supplied it should start with `~`,
  having nothing else on the left hand side. The right hand side may
  contain any valid formula syntax, such as `A + B` or `A + B + I(A^2)`,
  allowing non-linear effects. If this argument is defined, there is no
  need to define the `parents` argument. For example, using
  `parents=c("A", "B")` is equal to using `formula= ~ A + B`. May
  contain random effects and random slopes, in which case the simr
  package is used to generate the data. See details.

- betas:

  A numeric vector with length equal to `parents`, specifying the causal
  beta coefficients used to generate the node.

- intercept:

  A single number specifying the intercept that should be used when
  generating the node.

- error:

  A single number specifying the sigma error that should be used when
  generating the node. By setting this argument to 0, the linear
  predictor is returned directly. If `formula` contains mixed model
  syntax, this argument is passed to the `sigma` argument of the
  [`makeLmer`](https://rdrr.io/pkg/simr/man/makeGlmer.html) function of
  the simr package.

- var_corr:

  Variances and covariances for random effects. Only used when `formula`
  contains mixed model syntax. If there are multiple random effects,
  their parameters should be supplied as a named list. More complex
  structures are also supported. This argument is directly passed to the
  [`makeLmer`](https://rdrr.io/pkg/simr/man/makeGlmer.html) function of
  the simr package. Please consult the documentation of that package for
  more information on how mixed models should be specified. Some
  guidance can also be found in the "Issues" section of the official
  simr github page.

- link:

  The link function used to transform the linear predictor before adding
  the random error to it. For a standard linear regression model, this
  should be set to `link="identity"` (which is the default). Other
  allowed values are `"log"` and `"inverse"`, which are defined the same
  way as in the classic [`glm`](https://rdrr.io/r/stats/glm.html)
  function.

## Details

Using the general linear regression equation, the observation-specific
value that would be expected given the model is generated for every
observation in the dataset generated thus far. We could stop here, but
this would create a perfect fit for the node, which is unrealistic.
Instead, we add an error term by taking one sample of a normal
distribution for each observation with mean zero and standard deviation
`error`. This error term is then added to the predicted mean.

***Formal Description***:

Formally, the data generation can be described as:

\$\$Y \sim \texttt{intercept} + \texttt{parents}\_1 \cdot
\texttt{betas}\_1 + ... + \texttt{parents}\_n \cdot \texttt{betas}\_n+
N(0, \texttt{error}),\$\$

where \\N(0, \texttt{error})\\ denotes the normal distribution with mean
0 and a standard deviation of `error` and \\n\\ is the number of parents
(`length(parents)`).

For example, given `intercept=-15`, `parents=c("A", "B")`,
`betas=c(0.2, 1.3)` and `error=2` the data generation process is defined
as:

\$\$Y \sim -15 + A \cdot 0.2 + B \cdot 1.3 + N(0, 2).\$\$

When using a `link` other than `"identity"`, the procedure is
equivalent, except that the link function is applied to the linear
predictor before adding the random error term. For example, when using
`link="log"`, \\exp(-15 + A \cdot 0.2 + B \cdot 1.3) + N(0, 2)\\ is used
instead.

***Random Effects and Random Slopes***:

This function also allows users to include arbitrary amounts of random
slopes and random effects using the `formula` argument. If this is done,
the `formula`, and `data` arguments are passed to the variables of the
same name in the
[`makeLmer`](https://rdrr.io/pkg/simr/man/makeGlmer.html) function of
the simr package. The `fixef` argument of that function will be passed
the numeric vector `c(intercept, betas)` and the `VarCorr` argument
receives the `var_corr` argument as input. If used as a node type in a
`DAG`, all of this is taken care of behind the scenes. Users can simply
use the regular enhanced formula interface of the
[`node`](https://robindenz1.github.io/simDAG/reference/node.md) function
to define these formula terms, as shown in detail in the formula
vignette
([`vignette(topic="v_using_formulas", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_using_formulas.md)).
Please consult that vignette for examples. Also, please note that
inclusion of random effects or random slopes usually results in
significantly longer computation times.

## Author

Robin Denz

## Value

Returns a numeric vector of length `nrow(data)`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

set.seed(12455432)

# define a DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=12, error=2)

# define the same DAG, but with a pretty formula for the child node
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("bmi", type="gaussian", error=2,
       formula= ~ 12 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (1|School),
       var_corr=0.3, error=1)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=20)
}
```
