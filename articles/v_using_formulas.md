# Specifying Formulas in a DAG

## Introduction

In this small vignette, we give more detailed examples on how best to
use the `formula` argument in the
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) and
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
functions. This argument allows users to directly specify the full
structural equation that should be used to generate the respective node
in a clear and easy way, that does not directly rely on the `parents`,
`betas` and associated arguments. Note that the `formula` argument may
only be used with certain node types, as mentioned in the documentation.

## A simple example

We will start with a very simple example. Suppose we want to generate
some data from a simple DAG with no time-varying variables. Consider the
following DAG:

``` r
library(simDAG)

dag <- empty_dag() +
  node("A", type="rnorm", mean=0, sd=1) +
  node("B", type="rbernoulli", p=0.5, output="numeric") +
  node("C", type="rcategorical", probs=c(0.3, 0.2, 0.5),
       output="factor", labels=c("low", "medium", "high"))
```

This DAG contains only three root nodes of different types. $A$ is
normally distributed, $B$ is Bernoulli distributed and $C$ is a simple
categorical variable with the levels “low”, “medium” and “high”. If we
generate data from this DAG alone, it would look like this:

``` r
set.seed(23143)

dat <- sim_from_dag(dag, n_sim=10)
head(dat)
#>             A     B      C
#>         <num> <num> <fctr>
#> 1: -0.8041685     0    low
#> 2:  1.3390885     0 medium
#> 3:  0.9455804     0   high
#> 4: -2.3437852     1    low
#> 5: -0.9045554     1 medium
#> 6:  0.8532361     1 medium
```

Suppose we now want to generate an additional child node called $D$
which should be based on a linear regression model of the form:

$$D \sim - 8 + A \cdot 0.4 + B \cdot - 2 + N(0,1.5).$$

We could do this using the
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md)
function, by supplying appropriate values to the `parents`, `betas`,
`intercept` and `error` arguments. The following code could be used:

``` r
dag_without_formula <- dag +
  node("D", type="gaussian", parents=c("A", "B"), betas=c(0.4, -2),
       intercept=-8, error=1.5)
```

This does work just fine, but it may be a little cumbersome to specify
the DAG in this way. Since we want to use a linear regression model, we
could instead use the `formula` argument like this:

``` r
dag_with_formula <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + B*-2, error=1.5)
```

Given the same random number generator seed, the same output will be
produced from both DAGs, as shown below:

``` r
set.seed(34)
dat1 <- sim_from_dag(dag_without_formula, n_sim=100)

set.seed(34)
dat2 <- sim_from_dag(dag_with_formula, n_sim=100)

all.equal(dat1, dat2)
#> [1] TRUE
```

Formulas should always start with a `~` sign and have nothing else on
the left hand side. All parts of the formula should be connected by `+`
signs, never `-` signs. The name of the respective variable should
always be connected to the associated coefficient by a `*` sign. It does
not matter whether the name of the term or the coefficient go first, but
it has to be consistent in a formula. For example, `~ 1 + A*2 + B*3`
works, and `~ 1 + 2*A + 3*B` also works, but `~ 1 + 2*A + B*2` will
produce an error. The formula may also be supplied as a string and will
produce the same output.

Apart from being easier to read, this also allows the user a lot more
options. Through the use of formulas it is possible to specify nodes
that have categorical parents. It is also possible to include any order
of interaction effects and cubic terms using formulas, as shown below.

## Using a Categorical Parent Variable

Suppose that $D$ should additionally depend on $C$, a categorical
variable. For example, suppose this is the regression model we want to
generate data from:

$$D \sim - 8 + A \cdot 0.4 + B \cdot - 2 + Cmedium \cdot - 1 + Chigh \cdot - 3 + N(0,1.5).$$

In this model, the “low” category is used as a reference category. If
this is what we want to do, using the simple `parents`, `betas`,
`intercept` approach no longer works. We have to use a formula.
Fortunately, this is really simple to do using the following code:

``` r
dag2 <- dag +
  node("D", type="gaussian", error=1.5,
       formula=~ -8 + A*0.4 + B*-2 + Cmedium*-1 + Chigh*-3,
       parents=c("A", "B", "C"))
```

Essentially, all we have to do is use the name of the categorical
variable immediately followed by the category name. Note that if a
different reference category should be used, the user needs to re-define
the factor levels of the categorical variable accordingly first.

Note that we also defined the `parents` argument in this case. This is
not strictly necessary to generate the data in this case, but it is
recommended whenever categorical variables are used in a `formula` for
two reasons:

- **1.)** If `parents` is not specified, the
  [`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  function will not know that $C$ is a parent of $D$. If `sort_dag=TRUE`
  and/or the nodes are not specified in a correctly topologically sorted
  order, this may lead to errors when trying to generate the data.
- **2.)** If `parents` is not specified, other functions that take DAG
  objects as input (such as the
  [`plot.DAG()`](https://robindenz1.github.io/simDAG/reference/plot.DAG.md)
  function) may produce incorrect output, because they won’t know that
  $C$ is a parent of $D$.

## Using Interaction Effects

Interactions of any sort may also be added to the DAG. Suppose we want
to generate data from the following regression model:

$$D \sim - 8 + A \cdot 0.4 + B \cdot - 2 + A*B \cdot - 5 + N(0,1.5),$$

where $A*B$ indicates the interaction between $A$ and $B$. This can be
specified in the `formula` argument using the `:` sign:

``` r
dag3 <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + B*-2 + A:B*-5, error=1.5)
```

Since both $A$ and $B$ are coded as numeric variables here, this works
fine. If we instead want to include an interaction which includes a
categorical variable, we again have to use the name with the respective
category appended to it. For example, the following DAG includes an
interaction between $A$ and $C$:

``` r
dag4 <- dag +
  node("D", type="gaussian", error=1.5,
       formula=~ -8 + A*0.4 + B*-2 + Cmedium*-1 + Chigh*-3 + A:Cmedium*0.3 + 
         A:Chigh*10,
       parents=c("A", "B", "C"))
```

Higher order interactions may be specified in exactly the same way, just
using more `:` symbols. It may not always be obvious in which order the
variables for the interaction need to be specified. If the “wrong” order
was used, the
[`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function will return a helpful error message explaining which ones
should be used instead. For example, if we had used “Cmedium:A” instead
of “A:Cmedium”, this would not work because internally only the latter
is recognized as a valid column. Note that because $C$ is categorical,
we also specified the `parents` argument here just to be safe.

## Using Cubic Terms

Sometimes we also want to include non-linear relationships between a
continuous variable and the outcome in a data generation process. This
can be done by including cubic terms of that variable in a formula.
Suppose the regression model that we want to use has the following form:

$$D \sim - 8 + A \cdot 0.4 + A^{2} \cdot 0.02 + B \cdot - 2 + N(0,1.5).$$

The following code may be used to define such as node:

``` r
dag_with_formula <- dag +
  node("D", type="gaussian", formula= ~ -8 + A*0.4 + I(A^2)*0.02 + B*-2,
       error=1.5)
```

Users may of course use as many cubic terms as they like.

## Using Functions in formula

There is also direct support for including functions in the formula as
well. For example, it is allowed to call any function on the beta
coefficients, which is useful to specify betas on a different scale (for
example using Odds-Ratios instead of betas). For example:

``` r
dag_with_fun <- dag +
  node("D", type="binomial", formula= ~ -3 + A*log(0.5) + B*0.2)
```

is valid syntax. Any function can be used in the place of
[`log()`](https://rdrr.io/r/base/Log.html), as long as it is a single
function that is called on a beta-coefficient. It is also possible to
use functions on the variables themselves. However, it is required to
wrap them in a [`I()`](https://rdrr.io/r/base/AsIs.html) call. For
example, using something like `~ -3 + log(A)*0.5 + B*0.2` would not
work, but `~ -3 + I(log(A))*0.5 + B*0.2` is valid syntax. Although
supported in most cases, we strongly advise against using function names
with special characters. These might lead to weird errors when random
effects or random slopes are also contained in the `formula`.

## Using Special Characters in formula

Although not recommended, it is possible to use variable names
containing special characters in `formula`, by escaping them using the
usual R syntax. For example, if the user wanted to use `this-var` as a
variable name and use that variable as a parent node in a `formula`,
this could be done using the following code:

``` r
dag_with_fun <- dag +
  node("this-var", type="binomial", formula= ~ -3 + A*log(0.5) + B*0.2) +
  node("D", type="binomial", formula= ~ 5 + `this-var`*0.3)
```

There are, however, six special characters that may not be used in
`formula`: spaces, `+`, `*`, `(`, `)` and `|`. Errors may be produced
when using these characters in variable names, because they are used
internally to figure out which variables belong together and how (the
latter three mostly for mixed model syntax). It is best to avoid special
characters though, just to be safe.

## Using Random Effects and Random Slopes

Currently, three node types (`"gaussian"`, `"binomial"` and `"poisson"`)
directly allow the user to specify random effects and random slopes in
the `formula`. These should be specified exactly as they would be in a
regular call to the [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)
function. For example, consider the following `DAG`:

``` r
dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (1|School),
       error=1, var_corr=0.3)
```

Here, we have a `School` variable in which students of different `Age`s
are nested. To simulate the `Grade` of each student, we want to use a
random intercept per `School`, so we simply add the classic `(1|School)`
term to the `formula`. Whenever this is done, the `var_corr` argument
also has to be specified, which allows users to control the standard
deviation of the random effects. Internally, the
[`makeLmer()`](https://rdrr.io/pkg/simr/man/makeGlmer.html) and
[`doSim()`](https://rdrr.io/pkg/simr/man/doSim.html) functions form the
`simr` package are used to simulate these types of nodes. Please consult
the documentation of that package for details on how to specify more
complex mixed model structures, for example using multiple correlated
random effects.

In principle, arbitrary amounts of random effects can be added. Random
slopes can similarly be defined using the standard syntax. In the
following `DAG`, we use a random slope for `Age` per `School` in
addition to the random effect of `School`:

``` r
var_corr <- matrix(c(0.5, 0.05, 0.05, 0.1), 2)

dag_mixed <- empty_dag() +
  node("School", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("Age", type="rnorm", mean=12, sd=2) +
  node("Grade", type="gaussian", formula= ~ -2 + Age*1.2 + (Age|School),
       error=1, var_corr=var_corr)
```

Note that in this example, we defined the correlation between the random
effect and slopes using arbitrarily picked numbers using the `var_corr`
argument. Because of the much more complex DGP and the computational
overhead required to re-structure the data internally, simulations
including mixed model syntax is usually much slower than simulations
without them. In small samples this is not an issue, but very large
values for `n_sim` or using these specifications in discrete-time
simulations with large `max_t` may be difficult in some cases.

## Using External Coefficients (Advanced Usage)

Sometimes it may be useful to define the causal coefficients in external
variables, for example when writing a function that creates a `DAG`
objects with some set coefficients. This is supported through the use of
the [`eval()`](https://rdrr.io/r/base/eval.html) function as well. For
example:

``` r
beta_coef <- log(0.5)

dag_with_external <- dag +
  node("D", type="binomial", formula= ~ -3 + A*eval(beta_coef) + B*0.2)
```

is valid syntax. Note that this only works if the variable wrapped in
the [`eval()`](https://rdrr.io/r/base/eval.html) function call is
defined in the same environment in which the `DAG` object is being
created. If this is not the case, some weird error messages may be
produced, depending on the code used. Another option is to put the
formula together as a string before passing it to
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) like
this:

``` r
beta_coef <- log(0.5)

form_D <- paste("~ -3 + A*", beta_coef, "+ B*0.2")

dag_with_external <- dag +
  node("D", type="binomial", formula=form_D)
```

Since `formula` may always also be passed as a single string, this is
perfectly valid syntax and might allow users even more flexibility when
creating formulas inside functions.

## Using Formulas in Custom Node Types (Advanced Usage)

One of the great things about this package is that users can supply
*any* function to the `type` argument of
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) and
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md), as
long as it fulfills some minimal requirements (as described in
`?node_custom`). Whenever such a function is supplied, users may still
use the enhanced `formula` interface!

Internally, whenever an enhanced `formula` is supplied to
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md),
the formula is parsed to extract all variable names and
beta-coefficients. If cubic terms, interactions or levels of categorical
parent nodes are included in `formula`, the data will be re-structured
in a way that it includes a single column for each term in `formula`.
Consider the following example:

``` r
set.seed(123)

custom_fun <- function(data, parents, betas, intercept) {
  print(head(data))
  print(parents)
  print(betas)
  print(intercept)
  return(rep(1, nrow(data)))
}

dag_custom <- empty_dag() +
  node(c("A", "B"), type="rnorm", mean=0, sd=1) +
  node("C", type="rcategorical", probs=c(0.5, 0.5), labels=c("lev1", "lev2"))
```

Here, we defined a custom function for a node called `custom_fun`, which
really only returns 1, regardless of the input. However, it also prints
the first few lines of the input `data`, the `parents`, the `betas` and
the `intercept` it is passed, so that we can see whats going on under
the hood. Next, we defined some arbitrary `DAG` containing two numerical
root nodes and a binary root node. Here is what happens if we use
`custom_fun` with a `formula` input:

``` r
dag_custom2 <- dag_custom +
  node("Y", type=custom_fun, formula= ~ -5 + A*2 + B*-0.4)

data <- sim_from_dag(dag_custom2, n_sim=10)
#>              A          B
#>          <num>      <num>
#> 1: -0.56047565  1.2240818
#> 2: -0.23017749  0.3598138
#> 3:  1.55870831  0.4007715
#> 4:  0.07050839  0.1106827
#> 5:  0.12928774 -0.5558411
#> 6:  1.71506499  1.7869131
#> [1] "A" "B"
#> [1]  2.0 -0.4
#> [1] -5
```

We can see that it simply took the `data` that was required and printed
it. Note that in the
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md)
definition, we did not need to specify the `parents`, `betas` or
`intercept` arguments of the custom node, all we had to do was pass it
the enhanced `formula` and it happily extracted this information from it
and passed it to `custom_fun` directly. The only requirement for this to
work is that `custom_fun` does have these arguments. Now lets see what
happens with special terms in `formula`:

``` r
dag_custom2 <- dag_custom +
  node("Y", type=custom_fun, formula= ~ -5 + A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom2, n_sim=10)
#>             A          B         A:B     I(A^2) Clev2
#>         <num>      <num>       <num>      <num> <num>
#> 1:  0.4264642 -0.6947070 -0.29626767 0.18187173     0
#> 2: -0.2950715 -0.2079173  0.06135046 0.08706718     1
#> 3:  0.8951257 -1.2653964 -1.13268875 0.80124995     1
#> 4:  0.8781335  2.1689560  1.90463287 0.77111842     0
#> 5:  0.8215811  1.2079620  0.99243873 0.67499547     0
#> 6:  0.6886403 -1.1231086 -0.77341778 0.47422540     1
#> [1] "A"      "B"      "A:B"    "I(A^2)" "Clev2" 
#> [1]  2.0 -0.4  0.1 -0.1  0.2
#> [1] -5
```

Now we can see a slightly different picture. Instead of simply returning
the relevant parts of `data`, the function has re-structured it in a way
similar to the
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) function,
allowing us to directly apply the `betas` to it. Note that the `betas`
will always be in the right order, meaning that the first entry in
`betas` corresponds to the first term named in `parents`. The following
code is an example how one could re-build a linear regression using this
interface:

``` r
custom_linreg <- function(data, parents, betas, intercept) {
  intercept + rowSums(mapply("*", data, betas)) +
    rnorm(n=nrow(data), mean=0, sd=1)
}

dag_custom3 <- dag_custom +
  node("Y", type=custom_linreg, formula= ~ -5 + A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom3, n_sim=100)
head(data)
#>             A          B      C         Y
#>         <num>      <num> <char>     <num>
#> 1:  0.3796395  1.0527115   lev1 -4.841552
#> 2: -0.5023235 -1.0491770   lev1 -4.906313
#> 3: -0.3332074 -1.2601552   lev1 -4.857700
#> 4: -1.0185754  3.2410399   lev2 -7.542767
#> 5: -1.0717912 -0.4168576   lev2 -6.029375
#> 6:  0.3035286  0.2982276   lev2 -4.522188
```

Here, we simply calculate the linear predictor using the sum of the rows
after multiplying each value with the corresponding `betas` coefficient
and add a normally distributed error term with standard deviation of 1
afterwards. Of course users can use this type of strategy for much more
complex node types. Note that the intercept is not required to be there.
By omitting it from the supplied function, it will automatically no
longer be required in `formula` (and will be ignored if still
specified):

``` r
# same as before, but without an intercept (same as setting intercept=0)
custom_linreg2 <- function(data, parents, betas) {
  rowSums(mapply("*", data, betas)) +
    rnorm(n=nrow(data), mean=0, sd=1)
}

dag_custom4 <- dag_custom +
  node("Y", type=custom_linreg2, formula= ~ A*2 + B*-0.4 + A:B*0.1 + 
         I(A^2)*-0.1 + Clev2*0.2)

data <- sim_from_dag(dag_custom4, n_sim=100)
head(data)
#>             A          B      C          Y
#>         <num>      <num> <char>      <num>
#> 1:  0.8719650 -0.8338436   lev1  2.4685174
#> 2: -0.3484724  0.5787224   lev1 -0.3442883
#> 3:  0.5185038 -1.0875807   lev2  2.2053315
#> 4: -0.3906850  1.4840309   lev1 -3.1403262
#> 5: -1.0927872 -1.1862066   lev2 -1.1321410
#> 6:  1.2100105  0.1010792   lev2  3.4132667
```

Note that if you passed the `custom_linreg` function to the
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) call
above instead, you would receive an error message because of the missing
intercept. All of this of course also works with time-dependent nodes
specified using
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
calls.
