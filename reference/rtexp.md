# Sample values from a left-truncated exponential distribution

This function is a simple wrapper around the
[`rexp`](https://rdrr.io/r/stats/Exponential.html) function, allowing
users to directly sample values from a left-truncated exponential
distribution.

## Usage

``` r
rtexp(n, rate, l=NULL)
```

## Arguments

- n:

  How many draws to make.

- rate:

  A numeric vector of numbers \> 0, specifying the rate parameter of the
  exponential distribution.

- l:

  A numeric vector of numbers \> 0, specifying the value at which the
  distribution should be left-truncated.

## Author

Robin Denz

## Details

This function mostly exists so it can be used conveniently when
performing discrete-event simulations.

## Value

Returns a numeric vector of length `n`.

## Examples

``` r
library(simDAG)

rtexp(n=10, rate=0.05, l=20)
#>  [1] 81.29843 65.99487 35.53520 24.82525 37.02113 42.57515 30.69142 21.94689
#>  [9] 22.20097 22.25078

# without replacement
dag <- empty_dag() +
  node("A", type="rtexp", rate=0.01, l=100)
data <- sim_from_dag(dag, n_sim=5)
head(data)
#>           A
#>       <num>
#> 1: 232.8109
#> 2: 189.5243
#> 3: 533.3715
#> 4: 263.9551
#> 5: 326.7810
```
