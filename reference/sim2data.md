# Transform `sim_discrete_time` output into the start-stop, long- or wide-format

This function transforms the output of the
[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function into a single `data.table` structured in the start-stop format
(also known as counting process format), the long format (one row per
person per point in time) or the wide format (one row per person, one
column per point in time for time-varying variables). See details.

## Usage

``` r
sim2data(sim, to, use_saved_states=sim$save_states=="all",
         overlap=FALSE, target_event=NULL,
         keep_only_first=FALSE, remove_not_at_risk=FALSE,
         remove_vars=NULL, as_data_frame=FALSE,
         check_inputs=TRUE, ...)

# S3 method for class 'simDT'
as.data.table(x, keep.rownames=FALSE, to, overlap=FALSE,
              target_event=NULL, keep_only_first=FALSE,
              remove_not_at_risk=FALSE,
              remove_vars=NULL,
              use_saved_states=x$save_states=="all",
              check_inputs=TRUE, ...)

# S3 method for class 'simDT'
as.data.frame(x, row.names=NULL, optional=FALSE, to,
              overlap=FALSE, target_event=NULL,
              keep_only_first=FALSE, remove_not_at_risk=FALSE,
              remove_vars=NULL,
              use_saved_states=x$save_states=="all",
              check_inputs=TRUE, ...)
```

## Arguments

- sim, x:

  An object created with the
  [`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
  function.

- to:

  Specifies the format of the output data. Must be one of:
  `"start_stop"`, `"long"`, `"wide"`.

- use_saved_states:

  Whether the saved simulation states (argument `save_states` in
  `sim_discrete_time` function) should be used to construct the
  resulting data or not. See details.

- overlap:

  Only used when `to="start_stop"`. Specifies whether the intervals
  should overlap or not. If `TRUE`, the `"stop"` column is simply
  increased by one, as compared to the output when `overlap=FALSE`. This
  means that changes for a given \\t\\ are recorded at the start of the
  next interval, but the previous interval ends on that same day.

- target_event:

  Only used when `to="start_stop"`. By default (keeping this argument at
  `NULL`) all time-to-event nodes are treated equally when creating the
  start-stop intervals. This can be changed by supplying a single
  character string to this argument, naming one time-to-event node. This
  node will then be treated as the outcome. The output then corresponds
  to what would be needed to fit a Cox proportional hazards model. See
  details.

- keep_only_first:

  Only used when `to="start_stop"` and `target_event` is not `NULL`.
  Either `TRUE` or `FALSE` (default). If `TRUE`, all information after
  the first event per person will be discarded. Useful when
  `target_event` should be treated as a terminal variable.

- remove_not_at_risk:

  Only used when `to="start_stop"` and `target_event` is not `NULL`.
  Either `TRUE` or `FALSE` (default). If `TRUE`, the `event_duration`
  and `immunity_duration` of the `target_event` are taken into account
  when constructing the start-stop data. More precisely, the time in
  which individuals are not at-risk because they are either still
  currently experiencing the event or because they are immune to the
  event is removed from the start-stop data. This may be necessary when
  fitting some survival regression models, because these time-periods
  should not be counted as time at-risk.

- remove_vars:

  An optional character vector specifying which variables should *not*
  be included in the output. Set to `NULL` to include all variables
  included in the `sim` object (default).

- as_data_frame:

  Set this argument to `TRUE` to return a `data.frame` instead of a
  `data.table`.

- check_inputs:

  Whether to perform input checks (`TRUE` by default). Prints warning
  messages if the output may be incorrect due to missing information.

- keep.rownames:

  Currently not used.

- row.names:

  Passed to the `as.data.frame` function which is called on the finished
  `data.table`. See
  [`?as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) for more
  information.

- optional:

  Passed to the `as.data.frame` function which is called on the finished
  `data.table`. See
  [`?as.data.frame`](https://rdrr.io/r/base/as.data.frame.html) for more
  information.

- ...:

  Further arguments passed to `as.data.frame` (conversion from finished
  `data.table` to `data.frame`). Only available when directly calling
  `sim2data` with `as_data_frame=TRUE` or when using
  `as.data.frame.simDT`.

## Details

The raw output of the `sim_discrete_time` function may be difficult to
use for further analysis. Using one of these functions, it is
straightforward to transform that output into three different formats,
which are described below. Note that some caution needs to be applied
when using this function, which is also described below. Both
`as.data.table` and `as.data.frame` internally call `sim2data` and only
exist for user convenience.

***The start-stop format*:**

The start-stop format (`to="start_stop"`), also known as counting
process or period format corresponds to a `data.table` containing
multiple rows per person, where each row corresponds to a period of time
in which no variables changed. These intervals are defined by the
`start` and `stop` columns. The `start` column gives the time at which
the period started, the `stop` column denotes the time when the period
ended. By default these intervals are coded to be non-overlapping,
meaning that the edges of the periods are included in the period itself.
For example, if the respective period is exactly 1 point in time long,
`start` will be equal to `stop`. If non-overlapping periods are desired,
the user can specify `overlap=TRUE` instead.

By default, all time-to-event nodes are treated equally. This is not
optimal when the goal is to fit survival regression models. In this
case, we usually want the target event to be treated in a special way
(see for example Chiou et al. 2023). In general, instead of creating new
intervals for it we want existing intervals to end at event times with
the corresponding event indicator. This can be achieved by naming the
target outcome in the `target_event` variable. The previously specified
duration of this target event is then ignored. To additionally remove
all time periods in which individuals are not at-risk due to the event
still going on or them being immune to it (as specified using the
`event_duration` and `immunity_duration` parameters of
[`node_time_to_event`](https://robindenz1.github.io/simDAG/reference/node_time_to_event.md)),
users may set `remove_not_at_risk=TRUE`. If only the first occurrence of
the event is of interest, users may also set `keep_only_first=TRUE` to
keep only information up until the first event per person.

***The long format*:**

The long format (`to="long"`) corresponds to a `data.table` in which
there is one row per person per point in time. The unique person
identifier is stored in the `.id` column and the unique points in time
are given in the `.time` column.

***The wide format*:**

The wide format (`to="wide"`) corresponds to a `data.table` with exactly
one row per person and multiple columns per points in time for each
time-varying variable. All time-varying variables are coded as their
original variable name with an underscore and the time-point appended to
the end. For example, the variable `sickness` at time-point 3 is named
`"sickness_3"`.

***Output with `use_saved_states=TRUE`*:**

If `use_saved_states=TRUE`, this function will use only the data that is
stored in the `past_states` list of the `sim` object to construct the
resulting `data.table`. This results in the following behavior,
depending on which `save_states` option was used in the original
`sim_discrete_time` function call:

- `save_states="all"`: A complete `data.table` in the desired format
  with information for **all observations** at **all points in time**
  for **all variables** will be created. This is the safest option, but
  also uses the most RAM and computational time.

- `save_states="at_t"`: A `data.table` in the desired format with
  correct information for **all observations** at the **user specified
  times** (`save_states_at` argument) for **all variables** will be
  created. The state of the simulation at all other times will be
  ignored, because it wasn't stored. This may be useful in some
  scenarios, but is generally discouraged unless you have good reasons
  to use it. A warning message about this is printed if
  `check_inputs=TRUE`.

- `save_states="last"`: Since only the last state of the simulation was
  saved, an error message is returned. **No** `data.table` is produced.

***Output with `use_saved_states=FALSE`*:**

If `use_saved_states=FALSE`, this function will use only the data that
is stored in the final state of the simulation (`data` object in `sim`)
and information about `node_time_to_event` objects. If all `tx_nodes`
are `time_to_event` nodes or if all the user cares about are the
`time_to_event` nodes and time-fixed variables, this is the best option.

A `data.table` in the desired format with correct information about
`all observations` at `all times` is produced, but only with correct
entries for **some time-varying variables**, namely `time_to_event`
nodes. Note that this information will also only be correct if the user
used `save_past_events=TRUE` in all `time_to_event` nodes. Support for
`competing_events` nodes will be implemented in the future as well.

The other time-varying variables specified in the `tx_nodes` argument
will still appear in the output, but it will only be the value that was
observed at the last state of the simulation.

***Optional columns created using a `time_to_event` node*:**

When using a time-dependent node of type `"time_to_event"` with
`event_count=TRUE` or `time_since_last=TRUE`, the columns created using
either argument are **not** included in the output if `to="start_stop"`,
but will be included if `to` is set to either `"long"` or `"wide"`. The
reason for this behavior is that including these columns would lead to
nonsense intervals in the start-stop format, but makes sense in the
other formats.

***What about `tx_nodes` that are not `time_to_event` nodes?*:**

If you want the correct output for all `tx_nodes` and one or more of
those are not `time_to_event` nodes, you will have to use
`save_states="all"` in the original `sim_discrete_time` call. We plan to
add support for `competing_events` with other `save_states` arguments in
the near future. Support for arbitrary `tx_nodes` will probably take
longer.

## Note

Using the node names `"start"`, `"stop"`, `".id"`, `".time"` or names
that are automatically generated by time-dependent nodes of type
`"time_to_event"` may break this function.

## Value

Returns a single `data.table` (or `data.frame`) containing all simulated
variables in the desired format.

## Author

Robin Denz

## See also

[`sim_discrete_time`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)

## References

Sy Han Chiou, Gongjun Xu, Jun Yan, and Chiung-Yu Huang (2023).
"Regression Modeling for Recurrent Events Possibly with an Informative
Terminal Event Using R Package reReg". In: Journal of Statistical
Software. 105.5, pp. 1-34.

## Examples

``` r
library(simDAG)

set.seed(435345)

## exemplary car crash simulation, where the probability for
## a car crash is dependent on the sex, and the probability of death is
## highly increased for 3 days after a car crash happened
prob_car_crash <- function(data) {
  ifelse(data$sex==1, 0.001, 0.01)
}

prob_death <- function(data) {
  ifelse(data$car_crash_event, 0.1, 0.001)
}

dag <- empty_dag() +
  node("sex", type="rbernoulli", p=0.5) +
  node_td("car_crash", type="time_to_event", prob_fun=prob_car_crash,
          parents="sex", event_duration=3) +
  node_td("death", type="time_to_event", prob_fun=prob_death,
          parents="car_crash_event", event_duration=Inf)

# generate some data, only saving the last state
# not a problem here, because the only time-varying nodes are
# time-to-event nodes where the event times are saved
sim <- sim_discrete_time(dag, n_sim=20, max_t=500, save_states="last")

# transform to standard start-stop format
d_start_stop <- sim2data(sim, to="start_stop")
head(d_start_stop)
#>      .id start  stop car_crash  death    sex
#>    <int> <int> <num>    <lgcl> <lgcl> <lgcl>
#> 1:     1     1   114     FALSE  FALSE  FALSE
#> 2:     1   115   117      TRUE   TRUE  FALSE
#> 3:     1   118   417     FALSE   TRUE  FALSE
#> 4:     1   418   420      TRUE   TRUE  FALSE
#> 5:     1   421   481     FALSE   TRUE  FALSE
#> 6:     1   482   484      TRUE   TRUE  FALSE

# transform to "death" centric start-stop format
# and keep only information until death, cause it's a terminal event
# (this could be used in a Cox model)
d_start_stop <- sim2data(sim, to="start_stop", target_event="death",
                         keep_only_first=TRUE, overlap=TRUE)
head(d_start_stop)
#>      .id start  stop car_crash  death    sex
#>    <int> <int> <num>    <lgcl> <lgcl> <lgcl>
#> 1:     1     1   115     FALSE  FALSE  FALSE
#> 2:     1   115   116      TRUE   TRUE  FALSE
#> 3:     2     1    42     FALSE  FALSE  FALSE
#> 4:     2    42    45      TRUE  FALSE  FALSE
#> 5:     2    45   232     FALSE  FALSE  FALSE
#> 6:     2   232   235      TRUE  FALSE  FALSE

# transform to long-format
d_long <- sim2data(sim, to="long")
head(d_long)
#> Key: <.id, .time>
#>      .id .time car_crash  death    sex
#>    <int> <int>    <lgcl> <lgcl> <lgcl>
#> 1:     1     1     FALSE  FALSE  FALSE
#> 2:     1     2     FALSE  FALSE  FALSE
#> 3:     1     3     FALSE  FALSE  FALSE
#> 4:     1     4     FALSE  FALSE  FALSE
#> 5:     1     5     FALSE  FALSE  FALSE
#> 6:     1     6     FALSE  FALSE  FALSE

# transform to wide-format
d_wide <- sim2data(sim, to="wide")
#head(d_wide)
```
