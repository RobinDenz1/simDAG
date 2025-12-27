# Sample values from a given vector

This function is a simple wrapper around the
[`sample`](https://rdrr.io/r/base/sample.html) function, allowing users
to directly sample values from a given input vector (with or without
replacement and with or without defining selection probabilities) or
`data.frame` like object.

## Usage

``` r
rsample(n, x, replace=FALSE, prob=NULL)
```

## Arguments

- n:

  How many draws to make.

- x:

  A vector containing one or more elements from which to sample from, or
  a `data.frame` like object. If a `data.frame` is supplied, random rows
  from it will be sampled. Note that if the supplied `data.frame` has
  more than one column and this function is used as a node `type`, the
  names of the variables in the supplied `x` will be used as variable
  names and the given node name will be discarded.

- replace:

  Either `TRUE` or `FALSE`, specifying whether the sampling should be
  performed with or without replacement.

- prob:

  A numeric vector of probability weights for obtaining the elements of
  the vector being sampled or `NULL` (default). If `NULL`, a simple
  random sample without weights will be performed.

## Details

This function is very similar to the
[`rcategorical`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
function, with the main difference being that `rsample()` directly
supports any kind of vector input, not just a few categorical values,
but it does not support matrix input in the `prob` argument. Use
[`rcategorical`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
if the goal is to sample from a categorical distribution with few
categories or different probabilities per person and use `rsample()` for
general sampling purposes.

Note that this function is just a wrapper around the
[`sample`](https://rdrr.io/r/base/sample.html) function, with the only
additional functionality being that it also may be used to directly
sample from `data.frame`s. It is only meant to conveniently allow
sampling within the packages syntax (the original function does not use
the `n` argument, and can thus not be used directly without a wrapper).

## Author

Robin Denz

## Value

Returns a vector of length `n` with the same type as `x` if `x` is a
vector and a `data.frame` with `n` rows if `x` is a `data.frame`.

## Examples

``` r
library(simDAG)

# without replacement
dag <- empty_dag() +
  node("A", type="rsample", x=1:10, replace=FALSE)
data <- sim_from_dag(dag, n_sim=5)
head(data)
#>        A
#>    <int>
#> 1:     4
#> 2:     8
#> 3:     6
#> 4:     9
#> 5:     2

# with replacement and selection probabilities
dag <- empty_dag() +
  node("X", type="rbernoulli", p=0.5) +
  node("A", type="rsample", x=c(1, 2, 3, 4), replace=TRUE,
       prob=c(0.1, 0.3, 0.1, 0.5))
data <- sim_from_dag(dag, n_sim=100)
head(data)
#>         X     A
#>    <lgcl> <num>
#> 1:  FALSE     4
#> 2:  FALSE     2
#> 3:   TRUE     2
#> 4:   TRUE     1
#> 5:   TRUE     2
#> 6:  FALSE     4

# sampling rows from a data.frame object
# NOTE: The node name for the rsample() node will be ignored, because
#       a data.frame is supplied to "x". The names of the variables in the
#       data are used directly instead.
dag <- empty_dag() +
  node("placeholder", type="rsample", x=data) +
  node("Y", type="binomial", formula= ~ -2 + A*0.5 + X*-1)
data2 <- sim_from_dag(dag, n_sim=50)
head(data2)
#>         X     A      Y
#>    <lgcl> <num> <lgcl>
#> 1:   TRUE     2  FALSE
#> 2:  FALSE     4  FALSE
#> 3:  FALSE     2   TRUE
#> 4:  FALSE     4  FALSE
#> 5:  FALSE     1  FALSE
#> 6:   TRUE     4  FALSE
```
