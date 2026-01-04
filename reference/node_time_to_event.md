# Generate Data from repeated Bernoulli Trials in Discrete-Time Simulation

This node essentially models a dichotomous time-dependent variable for
which the time of the event will be important for later usage. It adds
two columns to `data`: `name_event` (whether the person currently has an
event) and `name_time` (the time at which the current event started).
Past events are stored in a list. Can only be used inside of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function, not outside of it. See details.

## Usage

``` r
node_time_to_event(data, parents, sim_time, past_states, name,
                   formula, prob_fun=NULL, ..., event_duration=1,
                   immunity_duration=event_duration, unif=NULL,
                   time_since_last=FALSE, event_count=FALSE,
                   save_past_events=TRUE, check_inputs=TRUE,
                   envir)
```

## Arguments

- data:

  A `data.table` containing all columns specified by `parents`. Similar
  objects such as `data.frame`s are not supported.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has. Those child nodes should be valid column
  names in `data`. Because the state of this variable is by definition
  dependent on its previous states, the columns produced by this
  function will automatically be considered its parents without the user
  having to manually specify this.

- sim_time:

  The current time of the simulation. If `sim_time` is an argument in
  the function passed to the `prob_fun` argument, this time will
  automatically be passed to it as well.

- past_states:

  A list of `data.table`s including previous states of the simulation.
  This argument cannot be specified directly by the user. Instead, it is
  passed to this function internally whenever a function is passed to
  the `prob_fun` argument which includes a named argument called
  `past_states`. May be useful to specify nodes that are dependent on
  specific past states of the simulation.

- name:

  The name of the node. This will be used as prefix before the `_event`,
  `_time` columns. If the `time_since_last` or `event_count` arguments
  are set to `TRUE`, this will also be used as prefix for those
  respective columns.

- formula:

  An optional enhanced formula, as used throughout the package. This may
  be used instead of the `prob_fun` argument, to specify a binomial
  regression model that should be used to calculate the probability
  instead. If specified (and `prob_fun=NULL`), the
  [`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  function is used with `return_prob=TRUE` to obtain the probabilities.
  If `prob_fun` is specified, this argument is ignored.

- prob_fun:

  A function that returns a numeric vector of size `nrow(data)`
  containing only numbers between 0 and 1. These numbers specify the
  person-specific probability of experiencing the event modeled by this
  node at the particular point in time of the simulation. The
  corresponding event will be generated internally using the
  [`rbernoulli`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
  function. The function needs to have a named argument called `data`.
  If the function has an argument named `sim_time`, the current
  simulation time will also be passed to this function automatically,
  allowing time-dependent probabilities to be generated. Alternatively
  this argument can be set to a single number (between 0 and 1),
  resulting in a fixed probability of occurrence for every simulated
  individual at every point in time.

- ...:

  An arbitrary amount of additional named arguments passed to `prob_fun`
  if `prob_fun` is specified, or to
  [`node_binomial`](https://robindenz1.github.io/simDAG/reference/node_binomial.md)
  if `formula` is specified and `prob_fun` is not. Ignore this if you do
  not want to pass any arguments. Also ignored if `prob_fun` is a single
  number.

- event_duration:

  A single number \> 0 specifying how long the event should last. The
  point in time at which an event occurs also counts into this duration.
  For example, if an event occurs at \\t = 2\\ and it has a duration of
  3, the event will be set to `TRUE` on \\t \in \\2, 3, 4\\\\.
  Therefore, all events must have a duration of at least 1 unit
  (otherwise they never happened).

- immunity_duration:

  A single number \>= `event_duration` specifying how long the person
  should be immune to the event after it is over. The count internally
  starts when the event starts, so in order to use an immunity duration
  of 10 time units after the event is over `event_duration + 10` should
  be used.

- unif:

  Specifies the (usually uniformly distributed) numeric vector that
  should be used to perform the Bernoulli trials. If `NULL` (default),
  the uniform numbers are generated internally at each point in time. If
  a single character string is supplied, a column with the same name in
  `data` will be used for these numbers (can, but does not need to be
  mentioned in `parents`). If a numeric vector is supplied directly,
  these values will be used instead. This argument may be useful to make
  two or more time-to-event nodes use the same "seed".

- time_since_last:

  Either `TRUE` or `FALSE` (default), indicating whether an additional
  column should be generated that tracks the number of time units since
  the individual had its last event onset. For example, if the
  individual experienced a single event at \\t = 10\\, this column would
  be `NA` before time 10, 0 at time 10 and increased by 1 at each point
  in time. If another event happens, the time is set to 0 again. The
  column is named `paste0(name, "_time_since_last")`. The difference to
  the column ending with `"_time"` is that this column will not be set
  to `NA` again if the `immunity_duration` is over. It keeps counting
  until the end of the simulation, which may be useful when constructing
  event-time dependent probability functions.

- event_count:

  Either `TRUE` or `FALSE` (default), indicating whether an additional
  column should be generated that tracks the number of events the
  individual has already experienced. This column is 0 for all
  individuals at t = 0. Each time a new event occurs, the counter is
  increased by one. Note that only new events increase this counter. For
  example, an individual with an event at t = 10 that has an
  `event_duration` of 15 will have a value of 0 before t = 10, and will
  have a value of 1 at t = 10 and afterwards. The column will be named
  `paste0(name, "_event_count")`.

- save_past_events:

  When the event modeled using this node is recurrent
  (`immunity_duration < Inf & event_duration < Inf`), the same person
  may experience multiple events over the course of the simulation.
  Those are generally stored in the `tte_past_events` list which is
  included in the output of the `sim_discrete_time` function. This
  extends the runtime and increases RAM usage, so if you are not
  interested in the timing of previous events or if you are using
  `save_states="all"` this functionality can be turned off by setting
  this argument to `FALSE`.

- check_inputs:

  Whether to perform plausibility checks for the user input or not. Is
  set to `TRUE` by default, but can be set to `FALSE` in order to speed
  things up when using this function in a simulation study or something
  similar.

- envir:

  Only used internally to efficiently store the past event times. Cannot
  be used by the user.

## Details

When performing discrete-time simulation using the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function, the standard node functions implemented in this package are
usually not sufficient because they don't capture the time-dependent
nature of some very interesting variables. Often, the variable that
should be modelled has some probability of occurring at each point in
time. Once it does occur, it has some kind of influence on other
variables for a period of time until it goes back to normal (or
doesn't). This could be a car crash, a surgery, a vaccination etc. The
`time_to_event` node function can be used to model these kinds of nodes
in a fairly straightforward fashion.

***How it Works***:

At \\t = 1\\, this node will be initialized for the first time. It adds
two columns to the data: `name_event` (whether the person currently has
an event) and `name_time` (the time at which the current event started)
where `name` is the name of the node. Additionally, it adds a list with
`max_t` entries to the `tte_past_events` list returned by the
`sim_discrete_time` function, which records which individuals
experienced a new event at each point in time.

In a nutshell, it simply models the occurrence of some event by
calculating the probability of occurrence at \\t\\ and drawing a single
bernoulli trial from this probability. If the trial is a "success", the
corresponding event column will be set to `TRUE`, the time column will
be set to the current simulation time \\t\\ and the column storing the
past event times will receive an entry.

The `_event` column will stay `TRUE` until the event is over. The
duration for that is controlled by the `event_duration` parameter. When
modeling terminal events such as death, one can simply set this
parameter to `Inf`, making the event eternal. In many cases it will also
be necessary to implement some kind of immunity after the event, which
can be done using the `immunity_duration` argument. This effectively
sets the probability of another occurrence of the event to 0 in the next
`immunity_duration` time steps. During the immunity duration, the event
may be `TRUE` (if the event is still ongoing) or `FALSE` (if the
`event_duration` has already passed). The `_time` column is similarly
set to the time of occurrence of the event and reset to `NA` when the
`immunity_duration` is over.

The probability of occurrence is calculated using the function provided
by the user using the `prob_fun` argument. This can be an arbitrary
complex function. The only requirement is that it takes `data` as a
first argument. The columns defined by the `parents` argument will be
passed to this argument automatically. If it has an argument called
`sim_time`, the current time of the simulation will automatically be
passed to it as well. Any further arguments can be passed using the
`...` syntax. A simple example could be a logistic regression node, in
which the probability is calculated as an additive linear combination of
the columns defined by `parents` (this could also be achieved more
cleanly using the `formula` argument). A more complex function could
include simulation-time dependent effects, further effects dependent on
past event times etc. Examples can be found below and in the vignettes.

***How it is Used***:

This function should never be called directly by the user. Instead, the
user should define a DAG object using the
[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md)
and [`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
functions and set the `type` argument inside of a `node_td` call to
`"time_to_event"`. This DAG can be passed to the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function to generate the desired data. Many examples and more
explanations are given below and in the vignettes of this package.

***What can be done with it***:

This type of node naturally supports the implementation of terminal and
recurrent events that may be influenced by pretty much anything. By
specifying the `parents` and `prob_fun` arguments correctly, it is
possible to create an event type that is dependent on past events of
itself or other time-to-event variables and other variables in general.
The user can include any amount of these nodes in their simulation. It
may also be used to simulate any kind of binary time-dependent variable
that one would usually not associate with the name "event" as well. It
is very flexible, but it does require the user to do some coding by
themselves (e.g. creating a suitable function for the `prob_fun`
argument).

***What can't be done with it***:

Currently this function only allows binary events. Categorical event
types may be implemented using the
[`node_competing_events`](https://robindenz1.github.io/simDAG/reference/node_competing_events.md)
function, which works in a very similar fashion.

## Author

Robin Denz, Katharina Meiszl

## Value

Returns a `data.table` containing at least two columns with updated
values of the node.

## Note

This function cannot be called outside of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function. It only makes sense to use it as a type in a
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function call, as described in the documentation and vignettes.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

## a simple terminal time-to-event node, with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("death", type="time_to_event", prob_fun=0.0001,
          event_duration=Inf)

## a simple recurrent time-to-event node with a constant probability of
## occurrence, independent of any other variable
dag <- empty_dag() +
  node_td("car_crash", type="time_to_event", prob_fun=0.001, event_duration=1)

## a time-to-event node with a time-dependent probability function that
## has an additional argument
prob_car_crash <- function(data, sim_time, base_p) {
  return(base_p + sim_time * 0.0001)
}

dag <- empty_dag() +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          event_duration=1, base_p=0.0001)

## a time-to-event node with a probability function dependent on a
## time-fixed variable
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex")

## a little more complex car crash simulation, where the probability for
## a car crash is dependent on the sex, and the probability of death is
## highly increased for 3 days after a car crash happened
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash_event, 0.1, 0.0001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex") +
  node_td("death", type="time_to_event", prob_fun=prob_death,
          parents="car_crash_event")

# use the sim_discrete_time function to simulate data from one of these DAGs:
sim <- sim_discrete_time(dag, n_sim=20, max_t=500)

# using a logistic regression model to specify the probability with the
# enhanced formula interface
dag <- empty_dag() +
    node(c("A", "B"), type="rnorm") +
    node_td("Y", type="time_to_event", formula= ~ -2 + A*1.2 + B*-0.2,
            event_duration=10)

## more examples can be found in the vignettes of this package
```
