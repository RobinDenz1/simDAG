# Specifying Custom Node Types in a DAG

## Introduction

In this small vignette, we give a detailed explanation on how to define
custom functions that can be used in the `type` argument of
[`node()`](https://robindenz1.github.io/simDAG/reference/node.md) or
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
calls. Although `simDAG` includes a large number of different node types
that can be used in this argument directly, it also allows the user to
pass any function to this argument, as long as that function meets some
limited criteria (as described below). This is an advanced feature that
most users probably don’t need for standard simulation studies. We
strongly recommend reading the documentation and the other vignettes
first, because this vignette assumes that the reader is already familiar
with the `simDAG` syntax and general features.

The support for custom functions in `type` allows users to create root
nodes, child nodes or time-dependent nodes that are not directly
implemented in this package. By doing so, users may create data with any
functional dependence they can think of. The requirements for each node
type are listed below. Some simple examples for each node type are given
in each section. If you think that your custom node type might be useful
to others, please contact the maintainer of this package via the
supplied e-mail address or github and we might add it to this package.

``` r
library(simDAG)

set.seed(1234)
```

## Root Nodes

### Requirements

Any function that generates some vector of size `n` with
`n==nrow(data)`, or a
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) with as many
rows as the current data can be used as a child node. The only
requirement is:

- **1.)** The function should have an argument called `n` which controls
  how many samples to generate.

Some examples that are already implemented in R outside of this package
are [`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html),
[`stats::rgamma()`](https://rdrr.io/r/stats/GammaDist.html) and
[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html). The function may
take any amount of further arguments, which will be passed through the
three-dot (`...`) syntax. Note that whenever the supplied function
produces a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or
similar object), the user has to ensure that the included columns are
named properly.

### Examples

Using external functions that fulfill the requirements which are already
defined by some other package can be done this way:

``` r
dag <- empty_dag() +
  node("A", type="rgamma", shape=0.1, rate=2) +
  node("B", type="rbeta", shape1=2, shape2=0.3)
```

Of course users may also define an appropriate root node function
themselves. The code below defines a function that takes the sum of a
normally distributed random number and a uniformly distributed random
number for each simulated individual:

``` r
custom_root <- function(n, min=0, max=1, mean=0, sd=1) {
  out <- runif(n, min=min, max=max) + rnorm(n, mean=mean, sd=sd)
  return(out)
}

# the function may be supplied as a string
dag <- empty_dag() +
  node("A", type="custom_root", min=0, max=10, mean=5, sd=2)

# equivalently, the function can also be supplied directly
# This is the recommended way!
dag <- empty_dag() +
  node("A", type=custom_root, min=0, max=10, mean=5, sd=2)

data <- sim_from_dag(dag=dag, n_sim=100)
head(data)
#>            A
#>        <num>
#> 1:  2.524972
#> 2: 10.058842
#> 3:  8.874968
#> 4:  9.203870
#> 5: 13.284535
#> 6: 12.529218
```

## Child Nodes

### Requirements

Again, almost any function may be used to generate a child node. Only
four things are required for this to work properly:

- **1.)** Its’ name should start with `node_` (if you want to use a
  string to define it in `type`).
- **2.)** It should contain an argument called `data` (contains the
  already generated data).
- **3.)** It should contain an argument called `parents` (contains a
  vector of the child nodes parents).
- **4.)** It should return either a vector of length `n_sim` or a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or similar
  object) with any number of columns and `n_sim` rows.

The function may include any amount of additional arguments specified by
the user.

### Examples

Below we define a custom child node type that is basically just a
gaussian node with some (badly done) truncation, limiting the range of
the resulting variable to be between `left` and `right`.

``` r
node_gaussian_trunc <- function(data, parents, betas, intercept, error,
                                left, right) {
  out <- node_gaussian(data=data, parents=parents, betas=betas,
                       intercept=intercept, error=error)
  out <- ifelse(out <= left, left,
                ifelse(out >= right, right, out))
  return(out)
}
```

Please note that this is a terrible form of truncation in most cases,
because it artificially distorts the resulting normal distribution at
the `left` and `right` values. It is only meant as an illustration. Here
is another example of a custom child node function, which simply returns
the sum of its parents:

``` r
parents_sum <- function(data, parents, betas=NULL) {
  out <- rowSums(data[, parents, with=FALSE])
  return(out)
}
```

We can use both of these functions in a DAG like this:

``` r
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("custom_1", type="gaussian_trunc", parents=c("sex", "age"),
       betas=c(1.1, 0.4), intercept=-2, error=2, left=10, right=25) +
  node("custom_2", type=parents_sum, parents=c("age", "custom_1"))

data <- sim_from_dag(dag=dag, n_sim=100)
head(data)
#>         age    sex custom_1 custom_2
#>       <num> <lgcl>    <num>    <num>
#> 1: 48.49105   TRUE 17.33651 65.82756
#> 2: 50.39048   TRUE 17.34963 67.74011
#> 3: 56.55498   TRUE 21.36313 77.91811
#> 4: 46.49763  FALSE 18.61867 65.11630
#> 5: 50.48704   TRUE 19.34207 69.82911
#> 6: 55.44852   TRUE 19.98135 75.42988
```

## Time-Dependent Nodes

### Requirements

By time-dependent nodes we mean nodes that are created using the
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
function. Currently, the
[`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function only supports nodes of type `"next_time"`, so the following
text is exclusively about time-dependent nodes in
[`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md).
In general, defining custom time-dependent nodes works in essentially
the same way as for simple root nodes or child nodes. The requirements
are:

- **1.)** Its’ name should start with `node_` (if you want to use a
  string to define it in `type`).
- **2.)** If it is a child node, it should contain an argument called
  `data` (contains the already generated data).
- **3.)** If it is a child node, it should contain an argument called
  `parents` (contains a vector of the child nodes parents). This is not
  necessary for nodes that are independently generated.
- **4.)** It should return either a vector of length `n_sim` or a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) (or similar
  object) with any number of columns and `n_sim` rows.

Again, any number of additional arguments is allowed and will be passed
through the three-dot syntax. Additionally, there are multiple build-in
arguments that users may specify in custom time-dependent nodes, which
are then used internally. First, users may add an argument to this
function called `sim_time`. If included in the function definition, the
current time of the simulation will be passed to the function on every
call made to it. Secondly, the argument `past_states` may be added. If
done so, a list containing all previous states of the simulation (as
saved using the `save_states` argument of the
[`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function) will be passed to it internally, giving the user access to the
data generated at previous points in time. Similarly, the arguments `n`
(corresponding to `n_sim`) or `dag` (corresponding to the supplied DAG)
may be used.

### Examples

#### Time-Dependent Root Nodes

An example for a custom time-dependent root node is given below:

``` r
node_custom_root_td <- function(data, n, mean=0, sd=1) {
  return(rnorm(n=n, mean=mean, sd=sd))
}
```

This function simply draws a new value from a normal distribution at
each point in time of the simulation. A DAG using this node type could
look like this:

``` r
n_sim <- 100

dag <- empty_dag() +
  node_td(name="Something", type=node_custom_root_td, n=n_sim, mean=10, sd=5)
#> Warning: Values for the arguments 'sim_time', 'past_states', 'dag' and 'n' are
#> passed internally whenever present in a node function ('type' argument of the
#> node_td() call). Any value specified in the node_td() call will be overwritten
#> during the simulation.
```

Equivalently, and more simply, we could use (because `n` is passed
automatically):

``` r
dag <- empty_dag() +
  node_td(name="Something", type=rnorm, mean=10, sd=5)
```

#### Time-Dependent Child Nodes

Below is an example for a function that can be used to define a custom
time-dependent child node:

``` r
node_custom_child <- function(data, parents) {
  out <- numeric(nrow(data))
  out[data$other_event] <- rnorm(n=sum(data$other_event), mean=10, sd=3)
  out[!data$other_event] <- rnorm(n=sum(!data$other_event), mean=5, sd=10)
  return(out)
}

dag <- empty_dag() +
  node_td("other", type="time_to_event", prob_fun=0.1) +
  node_td("whatever", type="custom_child", parents="other_event")
```

This function takes a random draw from a normal distribution with
different specifications based on whether a previously updated
time-dependent node called `other` is currently `TRUE` or `FALSE`.

#### Using the `sim_time` Argument

Below we give an example on how the `sim_time` argument may be used. The
following function simply returns the square of the current simulation
time as output:

``` r
node_square_sim_time <- function(data, sim_time, n_sim) {
  return(rep(sim_time^2, n=n_sim))
}

dag <- empty_dag() +
  node_td("unclear", type=node_square_sim_time, n_sim=100)
```

Note that we did not (and should not!) actually define the `sim_time`
argument in the
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
definition of the node, because it will be passed internally, just like
`data` is. As long as `sim_time` is a named argument of the function the
user is passing, it will be handled automatically. In real simulation
studies this feature may be used to create time-scale dependent risks or
effects for some time-dependent events of interest.

#### Using the `past_states` Argument

As stated earlier, another special kind of argument is the `past_states`
argument, which allows users direct access to past states of the
simulation. Below is an example of how this might be used:

``` r
node_prev_state <- function(data, past_states, sim_time) {
  if (sim_time < 3) {
    return(rnorm(n=nrow(data)))
  } else {
    return(past_states[[sim_time-2]]$A + rnorm(n=nrow(data)))
  }
}

dag <- empty_dag() +
  node_td("A", type=node_prev_state, parents="A")
```

This function simply returns the value used two simulation time steps
ago plus a normally distributed random value. To make this happen, we
actually use both the `sim_time` argument **and** the `past_states`
argument. Note that, again, we do not (and cannot!) define these
arguments in the
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
definition of the node. They are simply used internally.

A crucial thing to make the previous code work in an actual simulation
is the `save_states` argument of the
[`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function. This argument controls which states should be saved
internally. If users want to use previous states, these need to be
saved, so the argument should in almost all cases be set to
`save_states="all"`, as shown below:

``` r
sim <- sim_discrete_time(dag, n_sim=100, max_t=10, save_states="all")
```

## Using the Formula Interface

Users may also use the enhanced `formula` interface directly with custom
child nodes and custom time-dependent nodes. This is described in detail
in the vignette on specifying formulas (see
[`vignette(topic="v_using_formulas", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_using_formulas.md)).

## Some General Comments

Using custom functions as node types is an advanced technique to obtain
specialized simulated data. It is sadly impossible to cover all user
cases here, but we would like to give some general recommendations
nonetheless:

- When using custom nodes, pass the function to `type` directly, do not
  use a string. This might avoid some weird scoping issues, depending on
  which environment the simulation is performed in.
- Keep it simple, if u can. Particularly in time-dependent simulations,
  the computational complexity of the node function matters a lot.
- Consider if
  [`node_identity()`](https://robindenz1.github.io/simDAG/reference/node_identity.md)
  might be used instead. In many cases, it is a lot easier to just use a
  node of type `identity` instead of defining a new function.
- The structural equations printed for custom nodes may be
  uninformative. Keep this in mind when inspecting the output of
  `summary.DAG()`.
- In some special cases, your custom node function may require
  information from the supplied `dag` as well. If this is the case, just
  add a named argument called `dag` to your function and the supplied
  `dag` will be passed to it automatically whenever
  [`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
  or
  [`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  are used.
