# Sample values from a given vector

This function is a simple wrapper around the
[`sample`](https://rdrr.io/r/base/sample.html) function, allowing users
to directly sample values from a given input vector (with or without
replacement and with or without defining selection probabilities).

## Usage

``` r
rsample(n, x, replace=FALSE, prob=NULL)
```

## Arguments

- n:

  How many draws to make.

- x:

  A vector containing one or more elements from which to sample from.

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
[`sample`](https://rdrr.io/r/base/sample.html) function, with no
additional functionality or improvements. It is only meant to
conveniently allow sampling within the packages syntax (the original
function does not use the `n` argument, and can thus not be used
directly without a wrapper).

## Author

Robin Denz

## Value

Returns a vector of length `n` with the same type as `x`.

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
  node("A", type="rsample", x=c(1, 2, 3, 4), replace=TRUE,
       prob=c(0.1, 0.3, 0.1, 0.5))
data <- sim_from_dag(dag, n_sim=100)
head(data)
#>        A
#>    <num>
#> 1:     4
#> 2:     4
#> 3:     3
#> 4:     2
#> 5:     3
#> 6:     4
```
