# MRC HE DSL - mock user guide

Taking inspiration from [tidymodels workflows](https://workflows.tidymodels.org/articles/extras/getting-started.html).

## Workflow steps

1. Fit (logistic) regression model using raw study data (statistical model)
    - Use model predictions (`[0, 1]`, probabilities) to stratify data according to different *risk thresholds*, e.g. `> 5%`, `> 6%`, .... Actually in this case it the initial state occupancy probabilities/distribution.
    - Model fitting can be done with base R, Stan, BUGS or other external software

2. Create Markov model (economic model)
    - Defines states and transition probabilities of all possible health states a patient can be in
    - Parameter values are usually derived from inference (i.e. Step 1) or literature

3. Stratifications from **1.** are used for a given total population size to produce *starting state subpopulations* in the Markov model defined by **2.**
    - Simulate Markov model for each *scenario/stratification*

## Defining the workflow

```r
library(unifyHE)
```

The `unifyHE` package allows us to define a workflow for Health Economics modeling using a simple
interface. Generally, a workflow consists of a staticial and an economic model. The statistical
model is used to estimate disease progression and treatment effects from individual patient data.
The output of the statistical model is then used to parameterise the economic model, which is used
to simulate different scenarios at the population level. Finally, the simulated outcomes from the
economic model are used to inform decision making.

### Statistical model (inference)

*Note:* taken from [Getting Started • workflows](https://workflows.tidymodels.org/articles/extras/getting-started.html)

We first define the logistic regression model by defining which package or system we want to use to
fit the model. For simple cases such as this we can use the base R `glm` implementation, or we can
use external software packages such as *Stan* or *BUGS* for more advanced cases.

`unifyHE` provides a consistent framework for defining models and workflows, regardless of the underlying software used to fit the model.

```r
stat_mod <- logistic_reg(data) %>%
  set_engine("glm", ...) %>% # alternatives: stan, BUGS, ...
  generate_quantities(icd04 = sum(predict(data, type = "response") > 0.04),
                      icd06 = sum(predict(data, type = "response") > 0.06))
                             # this is like the generated parameters block in Stan code
                             # we'll need to transform the posteriors into what the simulation
                             # model needs as input, in this case the starting state probs
                             ##TODO: should this be R style or Stan code?
```

> This shows how the `unifyHE` package could interface with external tools such as Stan using a simple interface. The `...` argument is used to pass parameters to the external software.

The `generate_quantities` method is used to define any additional quantities that we want to
generate from the model. This is typically used to define the starting state probabilities for the
Markov model.

If *Stan* is used as the engine, we can define the model as follows:

```r
## Alternatively: read from txt file
stan_mod <- "
data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0,upper=1> y[N];
}

parameters {
  real alpha;
  real beta;
}

model {
  y ~ bernoulli_logit(alpha + beta * x);
}

generated_quantities {
    real pred;
    real p04;
    real p06;
    real icd04;
    real icd06;

    pred = bernoulli_logit_rng(alpha + beta * x);
    icd04 = sum(p04 > 0.04);
    icd06 = sum(p04 > 0.06);
}
"

logit_mod_stan <- logistic_reg() %>%
  set_engine("stan", model = stan_mod)
```

### Economic model

We define the Markov model using the `markov_model` function and configure the states and transtion
probabilitiew using `unifyHE`'s helper functions.

The initial states are defined using the `set_init_pop` function, which can take the generated
quanitites from the statistical model as input. Alternatively, the initial states can be manually defined.

```r
markov_mod <- markov_model() %>%
  # Use the previously fit statistical model to define the initial states
  set_init_pop(stat_mod) %>%
  # Alternatively, manually define the initial states
  # set_init_pop(icd04 = ..., icd06 = ...) %>%
  set_states(...) %>%
  set_transition_probs(...)
```

The Markov model can be visualised using [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html):

```r
autoplot(markov_mod)
```

> Demonstrates how we can provide visualisation tools to produce quick diagnostic plots.

### Simulation

Finally, we define a list of simulation parameters to run the actual simulation.

> This could just be a simple list, or a more specialised object if needed.

```r
sim_params <- list(t_max = 30, n_sim = 1000)
```

## Running the workflow

First we define the workflow

```r
he_workflow <- workflow() %>%
  add_model(stat_mod) %>%
  set_scenarios(init_pop = c(icd04, icd06)) %>%   # these are the input parameters that are different between scenarios
                                                  # because stat_mod could return others, like transition probs
  add_simulation(model = markov_mod, params = sim_params)
```

Then we can run the workflow on the input data

```r
he_result <- he_workflow %>%
  fit_model(data = input_data) %>%  # fit the statistical model
  run_simulation(data = input_data) # run the simulation
```

With this interface, we can easily run the same workflow on different datasets. The `he_workflow`
object is just another R object that can be saved and re-used later.

### Caching results

To reduce runtimes, we can cache the results of the statistical model using the `cache` parameter.

```r
he_result <- he_workflow %>%
  fit_model(data = input_data, cache = TRUE) %>%
  run_simulation(data = input_data)
```

This will store the results of the statistical model in a cache file on disk when it is not yet
present, or restore the results from the cache file if it is already present.

## Visualise results

> Another [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) method can be used to easily make a sensible default plot of the results.

```r
autoplot(he_result)
```
