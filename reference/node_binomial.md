# Generate Data from a (Mixed) Binomial Regression Model

Data from the parents is used to generate the node using binomial
regression (usually logistic regression) by predicting the covariate
specific probability and sampling from a Bernoulli distribution
accordingly. Allows inclusion of arbitrary random effects and slopes for
logistic models.

## Usage

``` r
node_binomial(data, parents, formula=NULL, betas, intercept,
              return_prob=FALSE, output="logical", labels=NULL,
              var_corr=NULL, link="logit")
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

- return_prob:

  Either `TRUE` or `FALSE` (default). If `TRUE`, the calculated
  probability is returned instead of the results of bernoulli trials.
  This argument is ignored if random effects or random slopes are
  specified in the `formula` input.

- output:

  A single character string, must be either `"logical"` (default),
  `"numeric"`, `"character"` or `"factor"`. If `output="character"` or
  `output="factor"`, the labels (or levels in case of a factor) can be
  set using the `labels` argument.

- labels:

  A character vector of length 2 or `NULL` (default). If `NULL`, the
  resulting vector is returned as is. If a character vector is supplied
  and `output="character"` or `output="factor"` is used, all `TRUE`
  values are replaced by the first entry of this vector and all `FALSE`
  values are replaced by the second argument of this vector. The output
  will then be a character variable or factor variable, depending on the
  `output` argument. This argument is ignored if `output` is set to
  `"numeric"` or `"logical"`.

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
  probability scale. For a standard logistic regression model, this
  should be set to `"logit"` (which is the default). Other allowed
  values are `"identity"`, `"probit"`, `"log"`, `"cloglog"` and
  `"cauchit"`, which are defined the same way as in the classic
  [`glm`](https://rdrr.io/r/stats/glm.html) function.

## Details

Using the normal form a logistic regression model, the observation
specific event probability is generated for every observation in the
dataset. Using the `rbernoulli` function, this probability is then used
to take one bernoulli sample for each observation in the dataset. If
only the probability should be returned `return_prob` should be set to
`TRUE`.

***Formal Description***:

Formally, the data generation (when using `link="logit"`) can be
described as:

\$\$Y \sim Bernoulli(logit(\texttt{intercept} + \texttt{parents}\_1
\cdot \texttt{betas}\_1 + ... + \texttt{parents}\_n \cdot
\texttt{betas}\_n)),\$\$

where \\Bernoulli(p)\\ denotes one Bernoulli trial with success
probability \\p\\, \\n\\ is the number of parents (`length(parents)`)
and the \\logit(x)\\ function is defined as:

\$\$logit(x) = ln(\frac{x}{1-x}).\$\$

For example, given `intercept=-15`, `parents=c("A", "B")` and
`betas=c(0.2, 1.3)` the data generation process is defined as:

\$\$Y \sim Bernoulli(logit(-15 + A \cdot 0.2 + B \cdot 1.3)).\$\$

It works the same way for other `link` functions, with the only
difference being that \\logit()\\ would be replaced.

***Output Format***:

By default this function returns a logical vector containing only `TRUE`
and `FALSE` entries, where `TRUE` corresponds to an event and `FALSE` to
no event. This may be changed by using the `output` and `labels`
arguments. The last three arguments of this function are ignored if
`return_prob` is set to `TRUE`.

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

Returns a logical vector (or numeric vector if `return_prob=TRUE`) of
length `nrow(data)`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

set.seed(5425)

# define needed DAG
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("age", "sex"),
       betas=c(1.1, 0.4), intercept=-2)

# define the same DAG, but using a pretty formula
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial",
       formula= ~ -2 + age*1.1 + sexTRUE*0.4)

# simulate data from it
sim_dat <- sim_from_dag(dag=dag, n_sim=100)

# returning only the estimated probability instead
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("age", "sex"),
       betas=c(1.1, 0.4), intercept=-2, return_prob=TRUE)

sim_dat <- sim_from_dag(dag=dag, n_sim=100)

## an example using a random effect
if (requireNamespace("simr")) {

library(simr)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="binomial", formula= ~ -10 + Age*1.2 + (1|School),
       var_corr=0.3)

sim_dat <- sim_from_dag(dag=dag_mixed, n_sim=100)
}
#> Loading required namespace: simr
#> Loading required package: lme4
#> Loading required package: Matrix
#> 
#> Attaching package: ‘simr’
#> The following object is masked from ‘package:lme4’:
#> 
#>     getData
#> The following object is masked from ‘package:igraph’:
#> 
#>     compare
```
