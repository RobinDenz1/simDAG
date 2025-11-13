# Use a single constant value for a root node

This is a small convenience function that simply returns the value
passed to it, in order to allow the use of a constant node as root node
in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function.

## Usage

``` r
rconstant(n, constant)
```

## Arguments

- n:

  The number of times the constant should be repeated.

- constant:

  A single value of any kind which is used as the only value of the
  resulting variable.

## Author

Robin Denz

## Value

Returns a vector of length `n` with the same type as `constant`.

## Examples

``` r
library(simDAG)

rconstant(n=10, constant=7)
#>  [1] 7 7 7 7 7 7 7 7 7 7

rconstant(n=4, constant="Male")
#> [1] "Male" "Male" "Male" "Male"
```
