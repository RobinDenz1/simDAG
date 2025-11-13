# Pearls do-operator for `DAG` objects

This function can be used to set one or more nodes in a given `DAG`
object to a specific value, which corresponds to an intervention on a
DAG as defined by the do-operator introduced by Judea Pearl.

## Usage

``` r
do(dag, names, values)
```

## Arguments

- dag:

  A `DAG` object created using the
  [`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
  and [`node`](https://robindenz1.github.io/simDAG/reference/node.md)
  functions. See
  [`?node`](https://robindenz1.github.io/simDAG/reference/node.md) for
  more information on how to specify a DAG.

- names:

  A character string specifying names of nodes in the `dag` object. The
  value of these nodes will be set to the corresponding value specified
  in the `values` argument. If the node is not already defined in `dag`,
  a new one will be added without warning.

- values:

  A vector or list of any values. These nodes defined with the `names`
  argument will be set to those values.

## Details

Internally this function simply removes the old node definition of all
nodes in `names` and replaces it with a new node definition that defines
the node as a constant value, irrespective of the original definition.
The same effect can be created by directly specifying the `DAG` in this
way from the start (see examples).

This function does not alter the original `DAG` in place. Instead, it
returns a modified version of the `DAG`. In other words, using only
`do(dag, names="A", values=3)` will not change the `dag` object.

## Value

Returns a `DAG` object with updated node definitions.

## Author

Robin Denz

## References

Judea Pearl (2009). Causality: Models, Reasoning and Inference. 2nd ed.
Cambridge: Cambridge University Press

## Examples

``` r
library(simDAG)

# define some initial DAG
dag <- empty_dag() +
  node("death", "binomial", c("age", "sex"), betas=c(1, 2), intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", parents=c("sex", "age"), type="binomial",
       betas=c(0.6, 0.2), intercept=-2)

# return new DAG with do(smoking = TRUE)
dag2 <- do(dag, names="smoking", values=TRUE)

# which is equivalent to
dag2 <- empty_dag() +
  node("death", "binomial", c("age", "sex"), betas=c(1, 2), intercept=-10) +
  node("age", type="rnorm", mean=10, sd=2) +
  node("sex", parents="", type="rbernoulli", p=0.5) +
  node("smoking", type="rconstant", constant=TRUE)

# use do() on multiple variables: do(smoking = TRUE, sex = FALSE)
dag2 <- do(dag, names=c("smoking", "sex"), values=list(TRUE, FALSE))
```
