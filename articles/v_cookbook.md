# simDAG Cookbook

## Introduction

The `simDAG` package can be used to generate all kinds of data, as shown
in the many examples and other vignettes of this package. In this
vignette we will further illustrate the capabilities of the package, by
giving very short and simplified examples on how to use `simDAG` to
generate multiple different kinds of data. The vignette can be seen as a
sort of “cookbook”, in the sense that it includes the building blocks
for many different possible data generation processes (DGP), which users
can expand on for their specific needs. Note that the examples shown are
not meant to be realistic, they are only meant to show the general
structure of the required code.

This vignette assumes that the reader is already somewhat familiar with
the `simDAG` syntax. More detailed explanations of how to simulate data
using a time-fixed DAG and DAGs including time-dependent variables is
given in other vignettes and the documentation of the functions. We
recommend consulting these vignettes first to get a feeling for the
required syntax before diving into these specific examples.

``` r
library(simDAG)
library(data.table)
library(survival)

set.seed(23414)
```

## Simulating Randomized Controlled Trials

First, we will give some examples on how data for randomized controlled
trials (RCT) could be generated. In a classic RCT, the treatment of
interest (below named `Treatment`) is randomly assigned to the
individuals of the study, making it a “root node” (a variable with no
direct causes) in the terminology of DAGs.

### Two Treatment Groups

Below is an example for an RCT with two treatment groups, a binary
outcome and two baseline covariates (`Age` and `Sex`):

``` r
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5) +
  node("Treatment", type="rbernoulli", p=0.5) +
  node("Outcome", type="binomial",
       formula= ~ -12 + Age*0.2 + Sex*1.1 + Treatment*-0.5)

data <- sim_from_dag(dag, n_sim=1000)
head(data)
#>         Age    Sex Treatment Outcome
#>       <num> <lgcl>    <lgcl>  <lgcl>
#> 1: 52.44476   TRUE      TRUE   FALSE
#> 2: 54.20709   TRUE     FALSE   FALSE
#> 3: 56.40461   TRUE      TRUE    TRUE
#> 4: 50.26406  FALSE     FALSE   FALSE
#> 5: 61.16070   TRUE      TRUE    TRUE
#> 6: 60.00081   TRUE      TRUE    TRUE
```

### Three or More Treatment Groups

The example from earlier can easily be made a little more complex, by
including three treatment groups instead of just two. This can be
achieved by using the
[`rcategorical()`](https://robindenz1.github.io/simDAG/reference/rcategorical.md)
function as node type instead of the
[`rbernoulli()`](https://robindenz1.github.io/simDAG/reference/rbernoulli.md)
function.

``` r
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5, output="numeric") +
  node("Treatment", type="rcategorical", probs=c(0.33333, 0.33333, 0.33333),
       labels=c("Placebo", "Med1", "Med2"), output="factor") +
  node("Outcome", type="binomial",
       formula= ~ -12 + Age*0.2 + Sex*1.1 + TreatmentMed1*-0.5 + 
         TreatmentMed2*-1)

data <- sim_from_dag(dag, n_sim=1000)
head(data)
#>         Age   Sex Treatment Outcome
#>       <num> <num>    <fctr>  <lgcl>
#> 1: 57.72406     1      Med1   FALSE
#> 2: 60.79561     0   Placebo    TRUE
#> 3: 58.59577     1   Placebo    TRUE
#> 4: 48.17102     1      Med1   FALSE
#> 5: 57.22410     0      Med1    TRUE
#> 6: 60.58842     0      Med2   FALSE
```

By setting `probs=c(0.33333, 0.33333, 0.33333)`, each treatment group is
choosen with a probability of 0.33333, meaning that the resulting groups
should be roughly of equal size in expectation.

### Multiple Outcome Measurements

The previous two examples assumed that there was a single fixed time at
which the binary outcome was measured. Below we switch things up a bit
by using a continuous outcome that is measured at 5 different points in
time after baseline. The syntax is nearly the same as before, with the
major difference being that we now use a
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
call to define the outcome, because it is time-dependent. Additionally,
because of this inclusion of a time-dependent node, we have to use the
[`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function instead of the
[`sim_from_dag()`](https://robindenz1.github.io/simDAG/reference/sim_from_DAG.md)
function. Finally, the
[`sim2data()`](https://robindenz1.github.io/simDAG/reference/sim2data.md)
has to be called to obtain the desired output in the long-format.

``` r
dag <- empty_dag() +
  node("Age", type="rnorm", mean=55, sd=5) +
  node("Sex", type="rbernoulli", p=0.5, output="numeric") +
  node("Treatment", type="rbernoulli", p=0.5) +
  node_td("Outcome", type="gaussian",
          formula= ~ -12 + Age*0.2 + Sex*1.1 + Treatment*-0.5, error=1)

sim <- sim_discrete_time(dag, n_sim=1000, max_t=5, save_states="all")
data <- sim2data(sim, to="long")
head(data)
#> Key: <.id, .time>
#>      .id .time      Age   Sex Treatment   Outcome
#>    <int> <int>    <num> <num>    <lgcl>     <num>
#> 1:     1     1 61.94304     1     FALSE 1.8466496
#> 2:     1     2 61.94304     1     FALSE 0.7907009
#> 3:     1     3 61.94304     1     FALSE 3.3109942
#> 4:     1     4 61.94304     1     FALSE 0.6094475
#> 5:     1     5 61.94304     1     FALSE 0.5008469
#> 6:     2     1 56.87963     1      TRUE 0.8499893
```

### Non-Compliance to Treatment Assignment

Previously we assumed that the treatment was assigned at baseline and
never changed throughout the study. In real RCTs, individuals are often
assigned to *treatment strategies* instead (for example: one pill per
week). Some individuals might not adhere to their assigned treatment
strategy, by for example “switching back” to not taking the pill.

The following code shows one possible way to simulate such data. In the
shown DGP, individuals are assigned to a treatment strategy at baseline.
Here, `Treatment = FALSE` refers to the control condition in which the
individual should not take any pills. In the intervention group
(`Treatment = TRUE`), however, the individual is assigned to take a pill
every week. On average, 5% of these individuals stop taking their pills
with each passing week. Once they stop, they never start taking them
again. The continuous outcome at each observation time depends on how
many pills each individual took in total. None of the individuals in the
control group start taking pills. For simplicity, the simulation is run
without any further covariates for 5 weeks.

``` r
# function to calculate the probability of taking the pill at t,
# given the current treatment status of the person
prob_treat <- function(data) {
  fifelse(!data$Treatment_event, 0, 0.95)
}

dag <- empty_dag() +
  node("Treatment_event", type="rbernoulli", p=0.5) +
  node_td("Treatment", type="time_to_event", parents=c("Treatment_event"),
          prob_fun=prob_treat, event_count=TRUE, event_duration=1) +
  node_td("Outcome", type="gaussian", formula= ~ -2 + 
            Treatment_event_count*-0.3, error=2)
sim <- sim_discrete_time(dag, n_sim=1000, max_t=5, save_states="all")
data <- sim2data(sim, to="long")
head(data)
#> Key: <.id, .time>
#>      .id .time Treatment    Outcome Treatment_event_count
#>    <int> <int>    <lgcl>      <num>                 <num>
#> 1:     1     1      TRUE -3.4368080                     1
#> 2:     1     2      TRUE -0.5167935                     2
#> 3:     1     3      TRUE -4.4188412                     3
#> 4:     1     4      TRUE -2.9598534                     4
#> 5:     1     5      TRUE -1.0813427                     5
#> 6:     2     1     FALSE -1.7865979                     0
```

What this DAG essentially means is that first, the `Treatment_event`
column is generated, which includes the assigned treatment at baseline.
We called it `Treatment_event` here instead of just `Treatment`, because
the value in this column should be updated with each iteration and nodes
of type `"time_to_event"` always split the node into two columns: status
and time. By setting `event_count=TRUE` in the
[`node_td()`](https://robindenz1.github.io/simDAG/reference/node.md)
call for the `Treatment` node, a count of the amount of pills taken up
to $t$ is directly calculated at each point in time (column
`Treatment_event_count`), which can then be used directly to generate
the `Outcome` node.

This simulation could be made more realistic (or just more complex), by
for example adding either of the following things:

- making the probability of switching dependent on the outcome at
  $t - 1$
- making the probability of switching dependent on other variables
- allowing patients to switch back to taking the pill after
  discontinuation
- allowing patients in the control group to switch to the treatment

These additions are left as an exercise to the user.

### With Cluster Randomization

Instead of randomly assigning *individuals* to the treatment, many
trials actually randomly assign the treatment at the level of clinics or
other forms of *clusters*, which is fittingly called cluster
randomization. This may be implemented in `simDAG` using the following
syntax:

``` r
dag <- empty_dag() +
  node("Clinic", type="rcategorical", probs=rep(0.02, 50)) +
  node("Treatment", type="identity", formula= ~ Clinic >= 25) +
  node("Outcome", type="poisson", formula= ~ -1 + Treatment*4 + (1|Clinic),
       var_corr=0.5)
data <- sim_from_dag(dag, n_sim=1000)
head(data)
#>    Clinic Treatment Outcome
#>     <int>    <lgcl>   <int>
#> 1:     16     FALSE       0
#> 2:     11     FALSE       0
#> 3:     41      TRUE      26
#> 4:     36      TRUE      61
#> 5:     30      TRUE      12
#> 6:     13     FALSE       0
```

In this DGP, each individual is randomly assigned to one of 50 Clinics
with equal probability. All patients in clinics numbered 0-24 are
assigned to the control group, while the other patients are assigned to
the treatment group. The outcome is then generated using a Poisson
regression with a random intercept based on the clinic.

## Simulating Observational Studies

In all previous examples, the variable of interest was a treatment that
was randomly assigned and did not depend on other variables (excluding
the `Clinic` for the cluster randomization example). In observational
studies, the variable of interest is usually not randomly assigned.
Below are some examples for generating such observational study data.

### Crossectional Data

In the following example, the treatment probability is dependent on both
a categorical and a continuous variable, which both also cause the
outcome:

``` r
dag <- empty_dag() +
  node("cat", type="rcategorical", probs=c(0.4, 0.2, 0.2),
       labels=LETTERS[1:3]) +
  node("cont", type="rbeta", shape1=0.2, shape2=1.2) +
  node("treatment", type="binomial",
       formula= ~ -0.2 + catB*0.3 + catC*1 + cont*0.2) +
  node("outcome", type="gaussian",
       formula= ~ 3 + catB*1.1 + catC*0.2 + cont*-0.1,
       error=1)
data <- sim_from_dag(dag, n_sim=100)
head(data)
#>       cat         cont treatment  outcome
#>    <char>        <num>    <lgcl>    <num>
#> 1:      B 0.0485645707      TRUE 3.862287
#> 2:      C 0.4544629918      TRUE 3.426110
#> 3:      B 0.0007678235     FALSE 4.866626
#> 4:      C 0.2277772334      TRUE 3.601459
#> 5:      B 0.4191175999     FALSE 2.892944
#> 6:      B 0.1157079692      TRUE 4.361019
```

If the goal was to estimate the causal effect of the treatment on the
outcome, we would need to adjust for both `cat` and `cont` here, using
for example a linear regression model (same as in the DGP) or something
like inverse probability of treatment weighting.

### Longitudinal Data

Here we give a small example for a longitudinal non-randomized study in
which the `treatment` at $t$ is dependent on past values of itself, and
on past values of the `outcome`. Additionally, the `outcome` is
dependent on past values of itself and on the current `treatment`. We
use the discrete-time simulation approach implemented in the
[`sim_discrete_time()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_time.md)
function to achieve this:

``` r
## function that generates the probability of treatment at t 
## for all individuals, given the current state of the simulation
prob_treat <- function(data, base_p, rr_treat, rr_outcome) {
  base_p * rr_treat^(data$treatment_event) * rr_outcome^(data$outcome_event)
}

## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_outcome <- function(data, base_p, rr_treat, rr_outcome) {
  base_p * rr_treat^(data$treatment_event) * rr_outcome^(data$outcome_event)
}

dag <- empty_dag() +
  node_td("treatment", type="time_to_event", prob_fun=prob_treat,
          parents=c("outcome_event"), event_duration=1,
          base_p=0.05, rr_treat=2, rr_outcome=0.5) +
  node_td("outcome", type="time_to_event", prob_fun=prob_outcome,
          parents=c("treatment_event"),
          event_duration=1, immunity_duration=40,
          base_p=0.01, rr_treat=0.3, rr_outcome=1.2)

sim <- sim_discrete_time(dag, n_sim=100, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE)
head(data)
#>      .id start  stop treatment outcome
#>    <int> <int> <num>    <lgcl>  <lgcl>
#> 1:     1     1     2      TRUE   FALSE
#> 2:     1     2     9     FALSE   FALSE
#> 3:     1     9    10      TRUE   FALSE
#> 4:     1    10    26     FALSE   FALSE
#> 5:     1    26    27      TRUE   FALSE
#> 6:     1    27    28     FALSE   FALSE
```

In this simulation, all individuals start out with no `treatment` and
without having experienced the `outcome`. The baseline probability to
get the treatment is 0.05 (`base_p=0.05`). It is twice as likely that
someone who got the `treatment` in the last period of time gets it again
in the next period of time (`rr_treat=2`), and it is much less likely
that someone who experienced the `outcome` at $t - 1$ will start
treatment (`rr_outcome=0.5`). Similarly, the baseline probability of the
`outcome` is `base_p=0.01`, with a much lower probability of
experiencing it when currently receiving the `treatment`
(`rr_treat=0.3`) and a slightly higher chance of experiencing it again
when having it at $t - 1$ (`rr_outcome=1.2`).

### Cox Model with Time-Varying Covariates

Below we give a small example for simulating data from a Cox model with
a time-dependent covariate using the discrete-time simulation approach.
In this example, there is one time-dependent covariate named `A` and a
single time-to-event outcome called `Y`.

``` r
## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_Y <- function(data, base_p, rr_treat) {
  base_p * rr_treat^(data$A_event)
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="time_to_event", prob_fun=prob_Y,
          parents=c("A_event"),
          event_duration=Inf, base_p=0.01, rr_treat=0.5)

sim <- sim_discrete_time(dag, n_sim=500, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE, target_event="Y",
                 keep_only_first=TRUE)
head(data)
#>      .id start  stop      A      Y
#>    <int> <int> <num> <lgcl> <lgcl>
#> 1:     1     1    86  FALSE   TRUE
#> 2:     2     1    14  FALSE  FALSE
#> 3:     2    14    34   TRUE  FALSE
#> 4:     2    34   120  FALSE   TRUE
#> 5:     3     1    27  FALSE  FALSE
#> 6:     3    27    47   TRUE  FALSE
```

More specifically, the treatment `A` has no causes but only a general
probability of being given (0.01). If it is given, it stays active for
20 days. During that time-frame, the probability of developing the
outcome `Y` is reduced by 50%. The time until the first occurrence of
`Y` can be described as a Cox proportional hazards model, where `A` is a
time-dependent variable:

$$\lambda(t) = \lambda_{0}(t)\exp\left( \beta_{A}A(t) \right)$$.

Here, $\lambda_{0}(t)$ is the baseline hazard, which in our example is
constant over time and equal to 0.01. The relative risk used in the
simulation can be recovered as the hazard ratio, e.g. it is equal to
$\exp\left( \beta_{A} \right)$. It can be recovered as:

``` r
mod <- coxph(Surv(start, stop, Y) ~ A, data=data)
summary(mod)
#> Call:
#> coxph(formula = Surv(start, stop, Y) ~ A, data = data)
#> 
#>   n= 1351, number of events= 495 
#> 
#>          coef exp(coef) se(coef)      z Pr(>|z|)    
#> ATRUE -0.9235    0.3971   0.1740 -5.308 1.11e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>       exp(coef) exp(-coef) lower .95 upper .95
#> ATRUE    0.3971      2.518    0.2824    0.5585
#> 
#> Concordance= 0.541  (se = 0.007 )
#> Likelihood ratio test= 36.56  on 1 df,   p=1e-09
#> Wald test            = 28.17  on 1 df,   p=1e-07
#> Score (logrank) test = 30.19  on 1 df,   p=4e-08
```

Note that the estimate above will not show an exact hazard ratio of 0.5,
because only 500 individuals were simulated. With increasing `n_sim`,
the estimated value will be closer to the true value of 0.5.

If continuous time is required, a nearly equivalent discrete-event
simulation may be used:

``` r
dag <- empty_dag() +
  node_td("A", type="next_time", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="next_time",
          formula= ~ log(0.01) + log(0.5)*A,
          event_duration=Inf)

data <- sim_discrete_event(dag, n_sim=500, max_t=500, target_event="Y",
                           keep_only_first=TRUE)
head(data)
#> Key: <.id, start>
#>      .id     start      stop      A      Y
#>    <int>     <num>     <num> <lgcl> <lgcl>
#> 1:     1   0.00000  77.60547  FALSE  FALSE
#> 2:     1  77.60547  97.60547   TRUE  FALSE
#> 3:     1  97.60547 121.82781  FALSE  FALSE
#> 4:     1 121.82781 141.82781   TRUE  FALSE
#> 5:     1 141.82781 197.83302  FALSE  FALSE
#> 6:     1 197.83302 217.83302   TRUE  FALSE
```

Here, we used nodes of type `"next_time"` instead, as required for
[`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
calls. Otherwise the specification is very similar. Note that instead of
using a custom probability function, we used the `formula` interface to
specify the probability of `Y`. This could have been done in the
discrete-time simulation as well, but for performance reasons it is
often better to use `prob_fun`.

Again, we can check the code by fitting the corresponding Cox model:

``` r
mod <- coxph(Surv(start, stop, Y) ~ A, data=data)
summary(mod)
#> Call:
#> coxph(formula = Surv(start, stop, Y) ~ A, data = data)
#> 
#>   n= 1396, number of events= 497 
#> 
#>          coef exp(coef) se(coef)      z Pr(>|z|)    
#> ATRUE -1.0892    0.3365   0.1863 -5.848 4.98e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#>       exp(coef) exp(-coef) lower .95 upper .95
#> ATRUE    0.3365      2.972    0.2336    0.4847
#> 
#> Concordance= 0.543  (se = 0.007 )
#> Likelihood ratio test= 47.01  on 1 df,   p=7e-12
#> Wald test            = 34.2  on 1 df,   p=5e-09
#> Score (logrank) test = 37.68  on 1 df,   p=8e-10
```

Which again shows a hazard ratio for `A` that is fairly close to 0.5, as
expected. Alternatively, we could have used `model="cox"` here to
directly use the
[`node_cox()`](https://robindenz1.github.io/simDAG/reference/node_cox.md)
functionality to generate the times for `Y`. This may be useful when
Weibull distributed baseline hazards should be used.

### Aalen Additive Hazards Model with Time-Dependent Covariates

Similar to the Cox model shown above, the Aalen additive hazards model
with a time-dependent covariate is defined as:

$$\lambda(t) = \lambda_{0}(t) + \beta_{A}A(t)$$

The main difference here is that the coefficients are directly on the
hazard difference scale and not on the log hazard ratio scale. Data from
such a model can be simulated by slightly adapting the `prob_Y()`
function:

``` r
## function that generates the probability of the outcome at t 
## for all individuals, given the current state of the simulation
prob_Y <- function(data, intercept, beta_treat) {
  h <- intercept + beta_treat*data$A_event
  return(1 - exp(-(h)))
}

dag <- empty_dag() +
  node_td("A", type="time_to_event", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="time_to_event", prob_fun=prob_Y,
          parents=c("A_event"),
          event_duration=Inf, intercept=0.001, beta_treat=0.05)

sim <- sim_discrete_time(dag, n_sim=500, max_t=500)
data <- sim2data(sim, to="start_stop", overlap=TRUE, target_event="Y",
                 keep_only_first=TRUE)
head(data)
#>      .id start  stop      A      Y
#>    <int> <int> <num> <lgcl> <lgcl>
#> 1:     1     1   133  FALSE  FALSE
#> 2:     1   133   153   TRUE  FALSE
#> 3:     1   153   476  FALSE  FALSE
#> 4:     1   476   490   TRUE   TRUE
#> 5:     2     1   148  FALSE  FALSE
#> 6:     2   148   168   TRUE  FALSE
```

The coefficients could be recovered using an `aalen()` model from the
`timereg` R package. The code is not shown to avoid an unneeded
dependency on that package.

Again, we could use the
[`sim_discrete_event()`](https://robindenz1.github.io/simDAG/reference/sim_discrete_event.md)
function instead, if we want to generate data from the same data
generation process but with continuous time:

``` r
dag <- empty_dag() +
  node_td("A", type="next_time", prob_fun=0.01,
          event_duration=20) +
  node_td("Y", type="next_time",
          formula= ~ 0.001 + A*0.05, model="aalen",
          event_duration=Inf)

data <- sim_discrete_event(dag, n_sim=500, max_t=500, target_event="Y",
                           keep_only_first=TRUE)
head(data)
#> Key: <.id, start>
#>      .id     start      stop      A      Y
#>    <int>     <num>     <num> <lgcl> <lgcl>
#> 1:     1   0.00000  27.46853  FALSE  FALSE
#> 2:     1  27.46853  40.18321   TRUE   TRUE
#> 3:     2   0.00000  14.72661  FALSE  FALSE
#> 4:     2  14.72661  34.39991   TRUE   TRUE
#> 5:     3   0.00000 274.71466  FALSE  FALSE
#> 6:     3 274.71466 288.78126   TRUE   TRUE
```

This specification uses the `model` argument to specify that an Aalen
additive hazards model should be used to generate the time until `Y`. By
also specifying a `formula`, the model is fully specified and times are
generated directly from
[`node_aalen()`](https://robindenz1.github.io/simDAG/reference/node_aalen.md).

## Miscellaneous Simulations

### Simulating Multi-Level Data

The `simDAG` package directly supports the inclusion of arbitrary mixed
model syntax in the `formula` interface of nodes of type `"gaussian"`,
`"binomial"` and `"poisson"`, which makes it relatively straightforward
to simulate multi-level data as well. In the following example, we
consider students that are nested in different schools. The outcome is a
continuous score of some kind.

``` r
dag <- empty_dag() +
  node("school", type="rcategorical", probs=rep(0.1, 10),
       labels=LETTERS[1:10]) +
  node("female", type="rbernoulli", p=0.5) +
  node("age", type="rnorm", mean=12, sd=3) +
  node("score", type="gaussian",
       formula= ~ -2 + female*3 + age*0.1 + (1|school),
       var_corr=0.5, error=1)
data <- sim_from_dag(dag, n_sim=10)
head(data)
#>    school female       age      score
#>    <char> <lgcl>     <num>      <num>
#> 1:      I   TRUE 12.957818  3.1832933
#> 2:      C   TRUE  7.924776  2.8786369
#> 3:      D   TRUE 10.176482  2.8363160
#> 4:      F  FALSE 10.180657  0.3773207
#> 5:      I  FALSE 10.157362 -1.2590350
#> 6:      C  FALSE 12.908317  0.8219375
```

In this example, there is a single random effect for school, with a
standard deviation of 0.5. This example could be expanded to include a
random slope for age per school, by exchanging `(1|school)` with
`(age|school)`, although we would then also need to adjust the
`var_corr` argument accordingly. More examples are given in the formula
vignette.

### Simulating Mixture Distributions

Instead of using random effects and random slopes, another possibility
to simulate a sort of “mixed” variables is to directly model it using
different regression models for different individuals. This can be done
using the `"mixture"` node type. Consider the following example:

``` r
dag <- empty_dag() +
  node("strata", type="rbernoulli", p=0.5) +
  node(c("var1", "var2"), type="rnorm", mean=0, sd=1) +
  node("Y", type="mixture", parents=c("strata", "var1", "var2"),
       distr=list(
         "strata==0", node(".", type="gaussian",
                           formula= ~ -2 + var1*2 + var2*-0.5, error=1),
         "strata==1", node(".", type="gaussian",
                           formula= ~ 5 + var1*-1 + var2*2.3, error=1.5)
       ))
data <- sim_from_dag(dag, n_sim=10)
head(data)
#>    strata        var1        var2          Y
#>    <lgcl>       <num>       <num>      <num>
#> 1:  FALSE -0.92575296  0.63422389 -3.7713404
#> 2:   TRUE  0.13016410 -0.08978755  5.8966310
#> 3:  FALSE -0.04944792 -0.47909778 -1.4346367
#> 4:  FALSE -1.33428562  0.08187681 -5.0422909
#> 5:  FALSE  0.20001205  0.03793749 -2.5544498
#> 6:  FALSE  0.82301745  1.84891790  0.3152809
```

Using the `distr` argument, we can easily define to which simulated
individuals the corresponding node definitions should be applied to. In
the example above, we defined different linear regression models for
individuals with `strata==0` and for individuals with `strata==1`. It
would also be possible to use entirely different node types etc.

### Simulating Outliers

One possibility to introduce a kind of outliers to a variable, is by
defining the variable that should contain it as a kind of mixture
distribution, made up of two parts, similar to how zero-inflated models
work. First, values are generated from the general distribution. Then,
if that variable exceeds some value, sample different values for it. A
very simple example:

``` r
dag <- empty_dag() +
  node(c("A", "B", "C"), type="rnorm") +
  node("Y", type="mixture", parents=c("A", "B", "C"),
       distr=list(
         "TRUE", node(".", type="gaussian", formula= ~ -2 + A*0.1 + B*1 + C*-2,
                      error=1),
         "Y > 3", node(".", type="rnorm", mean=10000, sd=500)
       ))
data <- sim_from_dag(dag, n_sim=10000)
```

Here, we first simulate 3 standard normal variables `A`, `B` and `C`,
which will be used as predictor variables for our desired outcome `Y`.
For `Y` itself, we use the `"mixture"` node type, which allows us to
define the node as a mix of multiple node types, based on some
conditions, which are generated from one by one. Because we set the
first condition to `TRUE`, the linear model next to it gets applied to
all individuals in the first run, generating values from the specified
linear model for all individuals. In the next run we condition on `Y`
itself, instructing the function to only simulate new values for
`Y > 3`. These values are drawn from a normal distribution with a very
high mean.

### Simulating Missing Values

Real data usually includes at least some missing values in key
variables. Below is a very simple example on how users could include
missing values in their data:

``` r
dag <- empty_dag() +
  node("A_real", type="rnorm", mean=10, sd=3) +
  node("A_missing", type="rbernoulli", p=0.5) +
  node("A_observed", type="identity",
       formula= ~ fifelse(A_missing, NA, A_real))

data <- sim_from_dag(dag, n_sim=10)
head(data)
#>       A_real A_missing A_observed
#>        <num>    <lgcl>      <num>
#> 1:  8.990730      TRUE         NA
#> 2:  8.742281     FALSE   8.742281
#> 3: 11.689047      TRUE         NA
#> 4: 10.882558     FALSE  10.882558
#> 5: 11.637514     FALSE  11.637514
#> 6:  5.727022     FALSE   5.727022
```

In this DAG, the real values of node `A` are generated first.
Afterwards, an indicator of whether the corresponding values is observed
is drawn randomly for each individual in node `A_missing`. The actually
observed value, denoted `A_observed`, is then generated by simply using
either the value of `A_real` or a simple `NA` if `A_missing==TRUE`. The
missingness shown here would correspond to **missing completely at
random** (MCAR) in the categorisation by XX. Although this might seem a
little cumbersome at first, it does allow quite a lot of flexibility in
the specification of different missingness mechanisms. For example, to
simulate **missing at random** (MAR) patterns, we could use the
following code instead:

``` r
dag <- empty_dag() +
  node("A_real", type="rnorm", mean=0, sd=1) +
  node("B_real", type="rbernoulli", p=0.5) +
  node("A_missing", type="rbernoulli", p=0.1) +
  node("B_missing", type="binomial", formula= ~ -5 + A_real*0.1) +
  node("A_observed", type="identity",
       formula= ~ fifelse(A_missing, NA, A_real)) +
  node("B_observed", type="identity",
       formula= ~ fifelse(B_missing, NA, B_real))

data <- sim_from_dag(dag, n_sim=10)
head(data)
#>        A_real B_real A_missing B_missing A_observed B_observed
#>         <num> <lgcl>    <lgcl>    <lgcl>      <num>     <lgcl>
#> 1:  0.5992828   TRUE     FALSE     FALSE  0.5992828       TRUE
#> 2:  0.2296464  FALSE     FALSE     FALSE  0.2296464      FALSE
#> 3: -1.1221036   TRUE     FALSE     FALSE -1.1221036       TRUE
#> 4: -0.5055837  FALSE     FALSE     FALSE -0.5055837      FALSE
#> 5:  0.2642537   TRUE     FALSE     FALSE  0.2642537       TRUE
#> 6:  0.7590262   TRUE     FALSE     FALSE  0.7590262       TRUE
```

Here, the missingness in `A_observed` is again MCAR, because it is
independent of everything. The missingness in `B_observed`, however, is
MAR because the probability of missingness is dependent on the actually
observed value of `A_real`.

### Simulating Measurement Error

Measurement error refers to situations in which variables of interest
are not measured perfectly. For example, the disease of interest may
only be detected in 90% of all patients with the disease and may falsely
be detected in 1% of all patients without the disease. The same strategy
shown for missing values could be used to simulate such data using
`simDAG`:

``` r
probs <- list(`TRUE`=0.9, `FALSE`=0.01)

dag <- empty_dag() +
  node("Disease_real", type="rbernoulli", p=0.5) +
  node("Disease_observed", type="conditional_prob", parents="Disease_real",
       probs=probs)

data <- sim_from_dag(dag, n_sim=10)
head(data)
#>    Disease_real Disease_observed
#>          <lgcl>           <lgcl>
#> 1:        FALSE            FALSE
#> 2:        FALSE            FALSE
#> 3:         TRUE             TRUE
#> 4:         TRUE             TRUE
#> 5:        FALSE            FALSE
#> 6:        FALSE            FALSE
```

In this example, the disease is present in 50% of all individuals. By
using a node with `type="conditional_prob"` we can easily draw new
values for the observed disease status by specifying the `probs`
argument correctly. We could similarly extend this example to make the
probability of misclassification dependent on another variable:

``` r
# first TRUE / FALSE refers to Sex = TRUE / FALSE
# second TRUE / FALSE refers to Disease = TRUE / FALSE
probs <- list(TRUE.TRUE=0.9, TRUE.FALSE=0.01,
              FALSE.TRUE=0.8, FALSE.FALSE=0.05)

dag <- empty_dag() +
  node("Sex", type="rbernoulli", p=0.5) +
  node("Disease_real", type="rbernoulli", p=0.5) +
  node("Disease_observed", type="conditional_prob",
       parents=c("Sex", "Disease_real"), probs=probs)

data <- sim_from_dag(dag, n_sim=1000)
head(data)
#>       Sex Disease_real Disease_observed
#>    <lgcl>       <lgcl>           <lgcl>
#> 1:  FALSE        FALSE            FALSE
#> 2:  FALSE         TRUE             TRUE
#> 3:  FALSE        FALSE            FALSE
#> 4:  FALSE        FALSE            FALSE
#> 5:  FALSE         TRUE            FALSE
#> 6:   TRUE         TRUE             TRUE
```

In this extended example, `Sex` is equally distributed among the
population (with `TRUE` = “female” and `FALSE` = “male”). In this
example, the probability of being diagnosed with the disease
(`Disease_observed`) if the disease is actually present is 0.9 for
females and only 0.8 for males. Similarly, the probability of being
diagnosed if the disease is not present is 0.01 for females and 0.05 for
males.
