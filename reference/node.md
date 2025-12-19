# Create a node object for a `DAG`

These functions should be used in conjunction with the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
function to create `DAG` objects, which can then be used to simulate
data using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions.

## Usage

``` r
node(name, type, parents=NULL, formula=NULL, ...)

node_td(name, type, parents=NULL, formula=NULL, ...)
```

## Arguments

- name:

  A character vector with at least one entry specifying the name of the
  node. If a character vector containing multiple different names is
  supplied, one separate node will be created for each name. These nodes
  are completely independent, but have the exact same node definition as
  supplied by the user. If only a single character string is provided,
  only one node is generated.

- type:

  A single character string specifying the type of the node. Depending
  on whether the node is a root node, a child node or a time-dependent
  node different node types are allowed. See details. Alternatively, a
  suitable function may be passed directly to this argument.

- parents:

  A character vector of names, specifying the parents of the node or
  `NULL` (default). If `NULL`, the node is treated as a root node. For
  convenience it is also allowed to set `parents=""` to indicate that
  the node is a root node.

- formula:

  An optional `formula` object to describe how the node should be
  generated or `NULL` (default). If supplied it should start with `~`,
  having nothing else on the left hand side. The right hand side should
  define the entire structural equation, including the betas and
  intercepts. It may contain any valid formula syntax, such as
  `~ -2 + A*3 + B*4` or `~ -2 + A*3 + B*4 + I(A^2)*0.3 + A:B*1.1`,
  allowing arbitrary non-linear effects, arbitrary interactions and
  multiple coefficients for categorical variables. Additionally, for
  some node types, random effects and random slopes are supported. If
  this argument is defined, there is no need to define the `betas` and
  `intercept` argument. The `parents` argument should still be specified
  whenever a categorical variable is used in the formula. This argument
  is supported for build-in nodes of type `"binomial"`, `"gaussian"`,
  `"poisson"`, `"negative_binomial"`, `"cox"`, `"aftreg"`, `"ahreg"`,
  `"ehreg"`, `"poreg"` and `"ypreg"` and for any custom node defined by
  the user. It is also supported for nodes of type `"identity"`, but
  slightly different input is expected in that case. See examples and
  the associated vignette for an in-depth explanation.

- ...:

  Further named arguments needed to specify the node. Those can be
  parameters of distribution functions such as the `p` argument in the
  [`rbernoulli`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
  function for root nodes or arbitrary named arguments such as the
  `betas` argument of the
  [`node_gaussian`](https://robindenz1.github.io/simDAG/reference/node_gaussian.md)
  function.

## Details

To generate data using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions, it is required to create a `DAG` object first. This object
needs to contain information about the causal structure of the data
(e.g. which variable causes which variable) and the specific structural
equations for each variable (information about causal coefficients, type
of distribution etc.). In this package, the `node` and/or `node_td`
functions are used in conjunction with the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
function to create this object.

This works by first initializing an empty `DAG` using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
function and then adding multiple calls to the `node` and/or `node_td`
functions to it using a simple `+`, where each call to `node` and/or
`node_td` adds information about a single node that should be generated.
Multiple examples are given below.

In each call to `node` or `node_td` the user needs to indicate what the
node should be called (`name`), which function should be used to
generate the node (`type`), whether the node has any parents and if so
which (`parents`) and any additional arguments needed to actually call
the data-generating function of this node later passed to the three-dot
syntax (`...`).

***`node` vs. `node_td`***:

By calling `node` you are indicating that this node is a time-fixed
variable which should only be generated once. By using `node_td` you are
indicating that it is a time-dependent node, which will be updated at
each step in time when using a discrete-time simulation, or at event
changes in discrete-event simulations.

`node_td` should only be used if you are planning to perform a
discrete-time or discrete-event simulation with the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
functions. `DAG` objects including time-dependent nodes may not be used
in the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function.

***Implemented Root Node Types***:

Any function can be used to generate root nodes. The only requirement is
that the function has at least one named argument called `n` which
controls the length of the resulting vector. For example, the user could
specify a node of type `"rnorm"` to create a normally distributed node
with no parents. The argument `n` will be set internally, but any
additional arguments can be specified using the `...` syntax. In the
`type="rnorm"` example, the user could set the mean and standard
deviation using `node(name="example", type="rnorm", mean=10, sd=5)`.

For convenience, this package additionally includes five custom
root-node functions:

- "[rbernoulli](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)":
  Draws randomly from a bernoulli distribution.

- "[rcategorical](https://robindenz1.github.io/simDAG/reference/rcategorical.md)":
  Draws randomly from any discrete probability density function.

- "[rsample](https://robindenz1.github.io/simDAG/reference/rsample.md)":
  Draws random samples from a given vector.

- "[rtexp](https://robindenz1.github.io/simDAG/reference/rtexp.md)":
  Draws random values from a left-truncated exponential distribution.

- "[rconstant](https://robindenz1.github.io/simDAG/reference/rconstant.md)":
  Used to set a variable to a constant value.

***Implemented Child Node Types***:

Currently, the following node types are implemented directly for
convenience:

- "[gaussian](https://robindenz1.github.io/simDAG/reference/node_gaussian.md)":
  A node based on (mixed) linear regression.

- "[binomial](https://robindenz1.github.io/simDAG/reference/node_binomial.md)":
  A node based on (mixed) binomial regression.

- "[conditional_prob](https://robindenz1.github.io/simDAG/reference/node_conditional_prob.md)":
  A node based on conditional probabilities.

- "[conditional_distr](https://robindenz1.github.io/simDAG/reference/node_conditional_distr.md)":
  A node based on conditional draws from different distributions.

- "[multinomial](https://robindenz1.github.io/simDAG/reference/node_multinomial.md)":
  A node based on multinomial regression.

- "[poisson](https://robindenz1.github.io/simDAG/reference/node_poisson.md)":
  A node based on (mixed) poisson regression.

- "[negative_binomial](https://robindenz1.github.io/simDAG/reference/node_negative_binomial.md)":
  A node based on negative binomial regression.

- "[zeroinfl](https://robindenz1.github.io/simDAG/reference/node_zeroinfl.md)":
  A node based on a zero-inflated poisson or negative binomial
  regression.

- "[identity](https://robindenz1.github.io/simDAG/reference/node_identity.md)":
  A node that is just some R expression of other nodes.

- "[mixture](https://robindenz1.github.io/simDAG/reference/node_mixture.md)":
  A node that is a mixture of different node definitions.

- "[cox](https://robindenz1.github.io/simDAG/reference/node_cox.md)": A
  node based on cox-regression.

- "[aftreg](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)":
  A node based on an accelerated failure time model.

- "[ahreg](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)":
  A node based on an accelerated hazard model.

- "[ehreg](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)":
  A node based on a extended hazard model.

- "[poreg](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)":
  A node based on a proportional odds model.

- "[ypreg](https://robindenz1.github.io/simDAG/reference/node_rsurv.md)":
  A node based on a Young and Prentice model.

For custom child node types, see below or consult the vignette on custom
node definitions.

***Implemented Time-Dependent Node Types***:

Currently, the following node types are implemented directly for
convenience to use in `node_td` calls:

- "[time_to_event](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)":
  A node based on repeatedly checking whether an event occurs at each
  point in time.

- "[competing_events](https://robindenz1.github.io/simDAG/reference/node_competing_events.md)":
  A node based on repeatedly checking whether one of multiple mutually
  exclusive events occurs at each point in time.

- "[next_time](https://robindenz1.github.io/simDAG/reference/node_next_time.md)":
  A node that draws the time of the next event in discrete-event
  simulation.

However, the user may also use any of the child node types in a
`node_td` call directly. For custom time-dependent node types, please
consult the associated vignette.

***Custom Node Types***

It is very simple to write a new custom `node_function` to be used
instead, allowing the user to use any `type` of data-generation
mechanism for any type of node (root / child / time-dependent). All that
is required of this function is, that it has the named arguments `data`
(the sample as generated so far) and, if it's a child node, `parents` (a
character vector specifying the parents) and outputs either a vector
containing `n_sim` entries, or a `data.frame` with `n_sim` rows and an
arbitrary amount of columns. More information about this can be found in
the associated vignette:
[`vignette(topic="v_custom_nodes", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_custom_nodes.md).

***Using child nodes as parents for other nodes***:

If the data generated by a child node is categorical (such as when using
`node_multinomial`) they can still be used as parents of other nodes for
most standard node types without issues. All the user has to do is to
use `formula` argument to supply an enhanced formula, instead of
defining the `parents` and `betas` argument directly. This works well
for all node types that directly support `formula` input and for all
custom nodes specified by the user. See the associated vignette:
[`vignette(topic="v_using_formulas", package="simDAG")`](https://robindenz1.github.io/simDAG/articles/v_using_formulas.md)
for more information on how to correctly use formulas.

***Cyclic causal structures***:

The name DAG (directed **acyclic** graph) implies that cycles are not
allowed. This means that if you start from any node and only follow the
arrows in the direction they are pointing, there should be no way to get
back to your original node. This is necessary both theoretically and for
practical reasons if we are dealing with static DAGs created using the
`node` function. If the user attempts to generate data from a static
cyclic graph using the
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function, an error will be produced.

However, in the realm of discrete-time or discrete-event simulations,
cyclic causal structures are perfectly reasonable. A variable \\A\\ at
\\t = 1\\ may influence a variable \\B\\ at \\t = 2\\, which in turn may
influence variable \\A\\ at \\t = 3\\ again. Therefore, when using the
`node_td` function to simulate time-dependent data using the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
or
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function, cyclic structures are allowed to be present and no error will
be produced.

## Note

Contrary to the R standard, this function does **NOT** support partial
matching of argument names. This means that supplying `nam="age"` will
not be recognized as `name="age"` and instead will be added as
additional node argument used in the respective data-generating function
call when using `sim_from_dag`.

## Value

Returns a `DAG.node` object which can be added to a `DAG` object
directly.

## Author

Robin Denz

## Examples

``` r
library(simDAG)

# creating a DAG with a single root node
dag <- empty_dag() +
  node("age", type="rnorm", mean=30, sd=4)

# creating a DAG with multiple root nodes
# (passing the functions directly to 'type' works too)
dag <- empty_dag() +
  node("sex", type=rbernoulli, p=0.5) +
  node("income", type=rnorm, mean=2700, sd=500)

# creating a DAG with multiple root nodes + multiple names in one node
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node(c("income_1", "income_2"), type="rnorm", mean=2700, sd=500)

# also using child nodes
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node("sickness", type="binomial", parents=c("sex", "income"),
       betas=c(1.2, -0.3), intercept=-15) +
  node("death", type="binomial", parents=c("sex", "income", "sickness"),
       betas=c(0.1, -0.4, 0.8), intercept=-20)

# creating the same DAG as above, but using the enhanced formula interface
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node("sickness", type="binomial",
       formula= ~ -15 + sexTRUE*1.2 + income*-0.3) +
  node("death", type="binomial",
       formula= ~ -20 + sexTRUE*0.1 + income*-0.4 + sickness*0.8)

# using time-dependent nodes
# NOTE: to simulate data from this DAG, the sim_discrete_time() function needs
#       to be used due to "sickness" being a time-dependent node
dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node("income", type="rnorm", mean=2700, sd=500) +
  node_td("sickness", type="binomial", parents=c("sex", "income"),
          betas=c(0.1, -0.4), intercept=-50)

# we could also use a DAG with only time-varying variables
dag <- empty_dag() +
  node_td("vaccine", type="time_to_event", prob_fun=0.001, event_duration=21) +
  node_td("covid", type="time_to_event", prob_fun=0.01, event_duration=15,
          immunity_duration=100)
```
