# Generate Data with Multiple Mutually Exclusive Events in Discrete-Time Simulation

This node essentially models a categorical time-dependent variable for
which the time and the type of the event will be important for later
usage. It adds two columns to `data`: `name_event` (which type of event
the person is currently experiencing) and `name_time` (the time at which
the current event started). Can only be used inside of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function, not outside of it. Past events and their kind are stored in
two lists. See details.

## Usage

``` r
node_competing_events(data, parents, sim_time, name,
                      prob_fun, ..., event_duration=c(1, 1),
                      immunity_duration=max(event_duration),
                      save_past_events=TRUE, check_inputs=TRUE,
                      envir)
```

## Arguments

- data:

  A `data.table` containing all columns specified by `parents`. Similar
  objects such as `data.frame`s are not supported.

- parents:

  A character vector specifying the names of the parents that this
  particular child node has.

- sim_time:

  The current time of the simulation.

- name:

  The name of the node. This will be used as prefix before the `_event`,
  `_time`, `_past_event_times` and `_past_event_kind` columns.

- prob_fun:

  A function that returns a numeric matrix with `nrow(data)` rows and
  one column storing probabilities of occurrence for each possible event
  type plus a column for no events. For example, if there are two
  possible events such as recurrence and death, the matrix would need to
  contain three columns. The first storing the probability of no-event
  and the other two columns storing probabilities for recurrence and
  death per person. Since the numbers are probabilities, the matrix
  should only contain numbers between 0 and 1 that sum to 1 in each row.
  These numbers specify the person-specific probability of experiencing
  the events modeled by this node at the particular point in time of the
  simulation. The corresponding event will be generated internally using
  the
  [`rcategorical`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
  function.

- ...:

  An arbitrary number of additional named arguments passed to
  `prob_fun`. Ignore this if you do not want to pass any arguments.

- event_duration:

  A numeric vector containing one positive integer for each type of
  event of interest, specifying how long that event should last. For
  example, if we are interested in modelling the time to a
  cardiovascular event with death as competing event, this argument
  would need 2 entries. One would specify the duration of the
  cardiovascular event and the other would be `Inf` (because death is a
  terminal event).

- immunity_duration:

  A single number \>= `max(event_duration)` specifying how long the
  person should be immune to all events after experiencing one. The
  count internally starts when the event starts, so in order to use an
  immunity duration of 10 time units after the event is over
  `max(event_duration) + 10` should be used.

- save_past_events:

  When the event modeled using this node is recurrent
  (`immunity_duration < Inf & any(event_duration < Inf)`), the same
  person may experience multiple events over the course of the
  simulation. Those are generally stored in the `ce_past_events` list
  and `ce_past_causes` list which are included in the output of the
  `sim_discrete_time` function. This extends the runtime and increases
  RAM usage, so if you are not interested in the timing of previous
  events or if you are using `save_states="all"` this functionality can
  be turned off by setting this argument to `FALSE`

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
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
node function can be used to model these kinds of nodes in a fairly
straightforward fashion.

This function is an extended version of the
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
function. Instead of simulating a binary event, it can generate multiple
competing events, where the occurrence of one event at time \\t\\ is
mutually exclusive with the occurrence of an other event at that time.
In other words, multiple events are possible, but only one can occur at
a time.

***How it Works***:

At \\t = 1\\, this node will be initialized for the first time. It adds
two columns to the data: `name_event` (whether the person currently has
an event) and `name_time` (the time at which the current event started)
where `name` is the name of the node. Additionally, it adds a list with
`max_t` entries to the `ce_past_events` list returned by the
`sim_discrete_time` function, which records which individuals
experienced a new event at each point in time. The `ce_past_causes` list
additionally records which kind of event happened at that time.

In a nutshell, it simply models the occurrence of some event by
calculating the probability of occurrence at \\t\\ and drawing a single
multinomial trial from this probability. If the trial is a "success",
the corresponding event column will be set to the drawn event type
(described using integers, where 0 is no event and all other events are
numbered consecutively), the time column will be set to the current
simulation time \\t\\ and the columns storing the past event times and
types will receive an entry.

The event column will stay at its new integer value until the event is
over. The duration for that is controlled by the `event_duration`
parameter. When modeling terminal events such as death, one can simply
set this parameter to `Inf`, making the event eternal. In many cases it
will also be necessary to implement some kind of immunity after the
event, which can be done using the `immunity_duration` argument. This
effectively sets the probability of another occurrence of the event to 0
in the next `immunity_duration` time steps. During the immunity
duration, the event may be `> 0` (if the event is still ongoing) or `0`
(if the `event_duration` for that event type has already passed).

The probability of occurrence is calculated using the function provided
by the user using the `prob_fun` argument. This can be an arbitrary
complex function. The only requirement is that it takes `data` as a
first argument. The columns defined by the `parents` argument will be
passed to this argument automatically. If it has an argument called
`sim_time`, the current time of the simulation will automatically be
passed to it as well. Any further arguments can be passed using the
`prob_fun_args` argument. A simple example could be a multinomial
logistic regression node, in which the probabilities are calculated as
an additive linear combination of the columns defined by `parents`. A
more complex function could include simulation-time dependent effects,
further effects dependent on past event times etc. Examples can be found
below and in the vignettes.

***What can be done with it***:

This type of node naturally support the implementation of competing
events, where some may be terminal or recurrent in nature and may be
influenced by pretty much anything. By specifying the `parents` and
`prob_fun` arguments correctly, it is possible to create an event type
that is dependent on past events of itself or other time-to-event
variables and other variables in general. The user can include any
amount of these nodes in their simulation. It may also be used to
simulate any kind of binary time-dependent variable that one would
usually not associate with the name "event" as well. It is very
flexible, but it does require the user to do some coding by themselves.

***What can't be done with it***:

This function may only be used to generate competing events, meaning
that the occurrence of event 1 at \\t = 1\\ makes it impossible for
event 2 at \\t = 1\\ to occur. If the user wants to generate multiple
events that are not mutually exclusive, he or she may add multiple
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
based nodes to the `dag` argument of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function.

In fact, a competing events node may be simulated using multiple calls
to the
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)
based nodes as well, by defining the `prob_fun` argument of these nodes
in such a way that the occurrence of event A makes the occurrence of
event B impossible. This might actually be easier to implement in some
situations, because it doesn't require the user to manually define a
probability function that outputs a matrix of subject-specific
probabilities.

## Author

Robin Denz

## Value

Returns a `data.table` containing the updated columns of the node.

## Note

This function cannot be called outside of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function. It only makes sense to use it as a type in a
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md)
function call, as described in the documentation and vignettes.

## See also

[`empty_dag`](https://robindenz1.github.io/simDAG/reference/empty_dag.md),
[`node`](https://robindenz1.github.io/simDAG/reference/node.md),
[`node_td`](https://robindenz1.github.io/simDAG/reference/node.md),
[`sim_from_dag`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md),
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## Examples

``` r
library(simDAG)

## a competing_events node with only terminal events, all with a constant
## probability of occurrence, independent of any other variable
prob_death_illness <- function(data) {

  # simply repeat the same probabilities for everyone
  n <- nrow(data)
  p_mat <- matrix(c(rep(0.9, n), rep(0.005, n), rep(0.005, n)),
                  byrow = FALSE, ncol=3)

  return(p_mat)
}

dag <- empty_dag() +
  node_td("death_illness", type="competing_events", prob_fun=prob_death_illness,
          event_duration=c(Inf, Inf))

## making one of the event-types terminal and the other recurrent
dag <- empty_dag() +
  node_td("death_illness", type="competing_events", prob_fun=prob_death_illness,
          event_duration=c(15, Inf))

## call the sim_discrete_time function to generate data from it
sim <- sim_discrete_time(dag, n_sim=100, max_t=500)

## more examples on how to use the sim_discrete_time function can be found
## in the documentation page of the node_time_to_event function and
## in the package vignettes
```
