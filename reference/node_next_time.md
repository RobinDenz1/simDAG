# Generate the Next Time of an Event in Discrete-Event Simulation

This node essentially models a dichotomous time-dependent variable for
which the time of the event will be important for later usage. Can only
be used inside of the
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function, not outside of it or in other simulation functions. See
details.

## Usage

``` r
node_next_time(data, formula, prob_fun, ...,
               distr_fun=rtexp, distr_fun_args=list(),
               model=NULL, event_duration=Inf,
               immunity_duration=event_duration,
               event_count=FALSE)
```

## Arguments

- data:

  A `data.table` containing all columns specified by `parents`. Similar
  objects such as `data.frame`s are not supported.

- formula:

  An optional `formula` that may be used to define the probability that
  will be passed to `distr_fun` using a binomial regression model. If
  supplied, the
  [`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  function will be called internally with `return_prob=TRUE`
  (calculating only the probability as estimated using the model). See
  [`?node`](https://robindenz1.github.io/simDAG/reference/node.md) or
  the associated vignette for more information about how a `formula`
  should be defined in this package. Note that
  [`net`](https://robindenz1.github.io/simDAG/reference/net.md) terms
  are currently not supported in this node type. This argument is
  ignored if `prob_fun` is specified.

- prob_fun:

  A function that returns a numeric vector of size `nrow(data)`. These
  numbers should be used to summarise the effect of the considered
  covariates per person. The summarised score is then used in the
  `distr_fun` function call that follows internally. For example, when
  using `distr_fun="rtexp"` (default), the `prob_fun` should generate
  the person-specific probability of experiencing the event during 1
  time unit. Any function may be used, as long as it has a named
  argument called `data`. Alternatively this argument can be set to a
  single number, resulting in a fixed summary score being used for every
  simulated individual at every point in time. The `formula` argument
  may be used as a convenient alternative if users want to specify a
  binomial regression model.

- ...:

  An arbitrary amount of additional named arguments passed to `prob_fun`
  if `prob_fun` is specified. If `formula` is specified and both
  `prob_fun` and `model` are not, these additional arguments are passed
  directly to the
  [`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  function instead. If `model` is specified, these arguments are passed
  to the respective model function. Ignore this if you do not want to
  pass any arguments. Also ignored if `prob_fun` is a single number.

- distr_fun:

  A function that returns the (left-truncated) next time at which the
  variable turns to `TRUE`. Any function that has at least three named
  arguments `n` (the number of times to draw), `rate` (the summary score
  returned by `prob_fun`) and `l` (the time of left-truncation) may be
  used. The function additionally needs to be vectorised over both
  `rate` and `l`, so that a vector of different values may be supplied.
  The left-truncation is required, so that it only generated times that
  are strictly larger than `l`. A classic example for such a function is
  the [`rtexp`](https://robindenz1.github.io/simDAG/reference/rtexp.md)
  function (the default). See examples and the associated vignette.

- distr_fun_args:

  A list of named arguments that should be passed to the function
  specified in the `distr_fun` argument.

- model:

  Alternative way to specify how the next time should be generated.
  Takes a single character string, specifying a time-to-event node or
  `NULL` (default) to not use this functionality. Currently supported
  values are `"cox"`, `"aalen"`, `"aftreg"`, `"ahreg"`, `"ehreg"`,
  `"poreg"` and `"ypreg"`. The node function corresponding to
  `paste0("node_", model)` is used then, so for example using
  `model="cox"` means that the
  [`node_cox`](https://robindenz1.github.io/simDAG/reference/node_cox.md)
  function is used to generate the next times. If this argument is
  specified, both `prob_fun` and `distr_fun` are ignored. Concurrent use
  of the `formula` argument is supported. Further arguments that need to
  be passed to the respective node function can be passed through the
  `...` syntax.

- event_duration:

  A single number \> 0 specifying how long the event should last. During
  this period, the corresponding variable is set to `TRUE`.

- immunity_duration:

  A single number \>= `event_duration` specifying how long the person
  should be immune to the event after it is over. The count internally
  starts when the event starts, so in order to use an immunity duration
  of 10 time units after the event is over `event_duration + 10` should
  be used. The corresponding variable is set to `FALSE` after the
  `event_duration` is up and until the `immunity_duration` is over.

- event_count:

  Either `TRUE` or `FALSE` (default), specifying whether an additional
  column should be added that counts the number of times this variable
  has been `TRUE` previously. If `TRUE`, the column will be named by
  taking the name of the node and appending `"_event_count"`. It is 0 at
  the beginning and increases by 1 at each point in time that the
  variable changes its status from `FALSE` to `TRUE`. Note that this may
  increase the time it takes to run the simulation. If the count of
  previous events is only needed for processing in the variable itself,
  a faster alternative is to keep this argument at `FALSE` and to use
  the internal `.event_count` column instead. Only use this argument if
  other variables should be dependent on the event count of this
  variable.

## Details

This function is the only time-dependent node type that may currently be
used when conducting discrete-event simulations using the
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function. It is very similar to the
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
function in spirit, as it is used to model a binary variable over time.
It is, however, not usable in
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
calls. Use the
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
function there instead.

***How it works***:

At the beginning (\\t = 0\\) of the simulation, any variable added using
this function is set to `FALSE` for all individuals. Then, the function
supplied in the `prob_fun` argument is applied to all individuals in the
current `data`, potentially using information from baseline covariates
and other time-dependent nodes (the latter of which are all `FALSE` at
this stage). The obtained summary score is then passed to the
`distr_fun` in order to generate the time at which the variable changes
from `FALSE` to `TRUE` for each individual.

For example, consider the situation in which only one time-dependent
variable is includded. In this case, the simulation time for each
individual jumps to the generated event time immediately. The variable
is then set to `TRUE`. Afterwards, the simulation time jumps until the
end of the `event_duration` (if that duration is `Inf`, the simulation
is over). The variable is then set back to `FALSE`. Next, the simulation
time jumps to the end of the `immunity_duration` (again, if this is
`Inf`, the simulation is over).

With more then one time-dependent variable, the situation is a little
more complicated. Consider two time-dependent variables `A` and `B`. At
\\t = 0\\, both are `FALSE` for every individual. The `prob_fun` and
subsequently the `distr_fun` of both variables are called to generate
the time of the next event in each of them. Lets say those are 20 and
42, respectively. The simulation time is then advanced to 20, setting
`A` to `TRUE`. At this point, the `prob_fun` and `distr_fun` arguments
are called again for `B`, because `B` might be dependent on current
values of `A`, drawing a new next event time for `B`. Crucially, this
time is drawn from a left-truncated `distr_fun`, so that it is always
larger than the current time of 20. Lets say that new time is 53.

The simulation is then advanced again, but not necessarily to 53. Lets
say the `event_duration` of `A` is only 10. In this case the simulation
time is only advanced to 30. `A` is then set to `FALSE` again and the
next time for `B` is re-computed using its `prob_fun` and `distr_fun`.
At this point, if the `immunity_duration` of `A` is not `Inf`, the next
time for `A` is also re-computed, left-truncated on the current
simulation time + the `immunity_duration`. Again, the time is advanced
to the next event and the cycle continues.

This process is repeated until either (1) all variables reach a terminal
state, (2) the simulation time for each individual is \>= `max_t`, (3) a
break condition defined by `break_if` is reached or (4) no individuals
are left after their removal through the `remove_if` argument.
Otherwise, the simulation runs forever.

***What can be done with it***:

This type of node naturally supports the implementation of terminal and
recurrent events that may be influenced by baseline variables and other
such events over time dynamically. By specifying the `parents` and
`prob_fun` arguments correctly, it is possible to create an event type
that is dependent on past events of itself or other time-to-event
variables and other variables in general, allowing non-markovian data to
be generated. The user can include any amount of these nodes in their
simulation. It may also be used to simulate any kind of binary
time-dependent variable that one would usually not associate with the
name "event" as well.

***What can't be done with it***:

Currently this function only allows binary events. Categorical event
types or continuous time-dependent variables are currently not
supported. The `event_duration` and `immunity_duration` can also only be
fixed for each node, and are not allowed to vary per person.

## Author

Robin Denz

## Value

This function is never actually called. It is only used so that the node
type `"next_time"` can be safely specified in
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
calls. It does not make sense to ever use it outside a
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md) call,
as it always returns `NULL`.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_discrete_event`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md),
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)

## Examples

``` r
library(simDAG)

## a simple terminal time-to-event node, with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("death", type="next_time", prob_fun=0.0001,
          event_duration=Inf)

## a simple recurrent time-to-event node with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("car_crash", type="next_time", prob_fun=0.001, event_duration=1)

## a next-time node with a probability function dependent on a
## time-fixed variable
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="next_time", prob_fun=prob_car_crash,
          parents="sex")

## a little more complex car crash simulation, where the probability for
## a car crash is dependent on the sex, and the probability of death is
## highly increased for 3 days after a car crash happened
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash, 0.1, 0.0001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="next_time", prob_fun=prob_car_crash,
          parents="sex", event_duration=3) +
  node_td("death", type="next_time", prob_fun=prob_death,
          parents="car_crash", event_duration=Inf)

# use the sim_discrete_time function to simulate data from one of these DAGs:
sim <- sim_discrete_event(dag, n_sim=20, max_t=500)

## more examples can be found in the vignettes of this package
```
