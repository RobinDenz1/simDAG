# Generate Data from a (Mixed) Poisson Regression Model

Data from the parents is used to generate the node using poisson
regression by predicting the covariate specific lambda and sampling from
a poisson distribution accordingly. Allows inclusion of arbitrary random
effects and slopes.

## Usage

``` r
node_poisson(data, parents, formula=NULL, betas, intercept,
             var_corr=NULL, link="log")
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

  The link function used to transform the linear predictor to the
  `lambda` value used in
  [`rpois`](https://rdrr.io/r/stats/Poisson.html). For a standard
  Poisson regression model, this should be set to `"log"` (which is the
  default). Other allowed values are `"identity"` and `"sqrt"`, which
  are defined the same way as in the classic
  [`glm`](https://rdrr.io/r/stats/glm.html) function.

## Details

Essentially, this function simply calculates the linear predictor
defined by the `betas`-coefficients, the `intercept` and the values of
the `parents`. The `link` function is then applied to this predictor and
the result is passed to the
[`rpois`](https://rdrr.io/r/stats/Poisson.html) function. The result is
a draw from a subject-specific poisson distribution, resembling the
user-defined poisson regression model.

***Formal Description***:

Formally, the data generation (using `link="log"`) can be described as:

\$\$Y \sim Poisson(\lambda),\$\$

where \\Poisson()\\ means that the variable is Poisson distributed with:

\$\$P\_\lambda(k) = \frac{\lambda^k e^{-\lambda}}{k!}.\$\$

Here, \\k\\ is the count and \\e\\ is eulers number. The parameter
\\\lambda\\ is determined as:

\$\$\lambda = \exp(\texttt{intercept} + \texttt{parents}\_1 \cdot
\texttt{betas}\_1 + ... + \texttt{parents}\_n \cdot
\texttt{betas}\_n),\$\$

where \\n\\ is the number of parents (`length(parents)`).

For example, given `intercept=-15`, `parents=c("A", "B")`,
`betas=c(0.2, 1.3)` the data generation process is defined as:

\$\$Y \sim Poisson(\exp(-15 + A \cdot 0.2 + B \cdot 1.3)).\$\$

***Random Effects and Random Slopes***:

This function also allows users to include arbitrary amounts of random
slopes and random effects using the `formula` argument. If this is done,
the `formula`, and `data` arguments are passed to the variables of the
same name in the
[`makeGlmer`](https://rdrr.io/pkg/simr/man/makeGlmer.html) function of
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

set.seed(345345)

dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="poisson",
       formula= ~ -2 + sexTRUE*1.1 + age*0.4)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="poisson", formula= ~ -2 + Age*1.2 + (1|School),
       var_corr=0.3)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=20)
}
```
