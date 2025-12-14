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
#>  [1] 34.34906 88.12827 40.70373 27.78129 43.35305 20.89357 52.47933 20.56222
#>  [9] 49.71915 49.57393

# without replacement
dag <- empty_dag() +
  node("A", type="rtexp", rate=0.01, l=100)
data <- sim_from_dag(dag, n_sim=5)
head(data)
#>           A
#>       <num>
#> 1: 129.1568
#> 2: 216.4785
#> 3: 207.6033
#> 4: 120.1542
#> 5: 235.8952
```
