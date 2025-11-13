# Generate Random Draws from a Bernoulli Distribution

A very fast implementation for generating bernoulli trials. Can take a
vector of probabilities which makes it very useful for simulation
studies.

## Usage

``` r
rbernoulli(n, p=0.5, output="logical", reference=NULL)
```

## Arguments

- n:

  How many draws to make.

- p:

  A numeric vector of probabilities, used when drawing the trials.

- output:

  A single character string, specifying which format the output should
  be returned as. Must be one of `"logical"` (default), `"numeric"`,
  `"character"` or `"factor"`.

- reference:

  A single character string, specifying which of the two possible values
  should be considered the reference when `output="factor"` (ignored
  otherwise).

## Details

Internally, it uses only a single call to `runif`, making it much faster
and more memory efficient than using `rbinomial`.

Note that this function accepts values of `p` that are smaller then 0
and greater than 1. For `p < 0` it will always return `FALSE`, for
`p > 1` it will always return `TRUE`.

## Author

Robin Denz

## Value

Returns a vector of length `n` in the desired output format.

## Examples

``` r
library(simDAG)

# generating 5 bernoulli random draws from an unbiased coin
rbernoulli(n=5, p=0.5)
#> [1] FALSE FALSE  TRUE FALSE FALSE

# using different probabilities for each coin throw
rbernoulli(n=5, p=c(0.1, 0.2, 0.3, 0.2, 0.7))
#> [1] FALSE FALSE  TRUE FALSE FALSE

# return as numeric instead
rbernoulli(n=5, p=0.5, output="numeric")
#> [1] 1 0 0 1 1
```
