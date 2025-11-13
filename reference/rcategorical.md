# Generate Random Draws from a Discrete Set of Labels with Associated Probabilities

Allows different class probabilities for each person by supplying a
matrix with one column for each class and one row for each person.

## Usage

``` r
rcategorical(n, probs, labels=NULL, output="numeric",
             reference=NULL)
```

## Arguments

- n:

  How many draws to make. Passed to the `size` argument of the `sample`
  function if `probs` is not a matrix.

- probs:

  Either a numeric vector of probabilities which sums to one or a matrix
  with one column for each desired class and `n` rows. Passed to the
  `probs` argument of the `sample` function if a numeric vector is
  passed.

- labels:

  A vector of labels to draw from. If `NULL` (default), it simply uses
  integers starting from 1. Passed to the `x` argument of the `sample`
  function if `probs` is not a matrix.

- output:

  A single character string specifying the output format of the results.
  Must be either `"numeric"` (default), `"character"` or `"factor"`. If
  labels are supplied, the output will be parsed as characters by
  default.

- reference:

  A single character string, specifying which of the possible values
  should be considered the reference when `output="factor"` (ignored
  otherwise).

## Details

In case of a simple numeric vector (class probabilities should be the
same for all draws), this function is only a wrapper for the `sample`
function, to make the code more consistent. It uses weighted sampling
with replacement. Otherwise, custom code is used which is faster than
the standard `rmultinom` function.

## Author

Robin Denz

## Value

Returns a numeric vector (or factor vector if `coerce2factor=TRUE`) of
length `n`.

## Examples

``` r
library(simDAG)

rcategorical(n=5, labels=c("A", "B", "C"), probs=c(0.1, 0.2, 0.7))
#> [1] "C" "C" "C" "A" "C"

rcategorical(n=2, probs=matrix(c(0.1, 0.2, 0.5, 0.7, 0.4, 0.1), nrow=2))
#> [1] 1 2
```
