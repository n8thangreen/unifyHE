# MRC HE DSL - mock user guide

Taking inspiration from [tidymodels workflows](https://workflows.tidymodels.org/articles/extras/getting-started.html).

## Workflow steps

1. Fit (logistic) regression model using raw study data
    - Use model predictions (`[0, 1]`, probabilities) to stratify data according to different *risk thresholds*, e.g. `> 5%`, `> 6%`, .... Actually in this case it the initial state occupancy probabilities/distribution.
    - Model fitting can be done with base R, Stan, BUGS or other external software

2. Create Markov model
    - Defines states and transition probabilities of all possible health states a patient can be in
    - Parameter values are usually derived from inference (i.e. Step 1) or literature

3. Stratifications from **1.** are used for a given total population size to produce *starting state subpopulations* in the Markov model defined by **2.**
    - Simulate Markov model for each *scenario/stratification*

## Defining the workflow

```r
library(unifyHE)
```

### Logistic regression

*Note:* taken from [Getting Started • workflows](https://workflows.tidymodels.org/articles/extras/getting-started.html)

We first define the logistic regression model by defining which package or system we want to use to fit the model. For simple cases such as this we can use the base R `glm` implementation, or we can use external software packages such as *Stan* or *BUGS* for more advanced cases.

> This showcases how the `unifyHE` package could interface with external tools such as Stan using a simple interface. The `...` argument is used to pass parameters to the external software.

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

### Markov model

Set up the base structure of the Markov model.

```r
markov_mod <- markov_model() %>%
  set_init_pop("init_pop") %>%
  set_states(...) %>%
  set_transition_probs(...)
```

The Markov model can be visualised using [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html):

```r
autoplot(markov_mod)
```

> Demonstrates how we can provide visualisation tools to produce quick diagnostic plots.

### Simulation

Define the simulation parameters.

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
  run_simulation(data = input_data)
```

NG: can we cache runs so that the inference doesnt have to be rerun?

## Visualise results

> Another `autoplot` method can be used to easily make a sensible default plot of the results.

```r
autoplot(he_result)
```
