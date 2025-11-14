## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "80%", fig.width = 7, fig.height = 5,
  fig.align = "center"
)

## ----setup-sir----------------------------------------------------------------
library(epiworldR)

model_seed <- 122

model_sir <- ModelSIR(
  name = "COVID-19",
  prevalence = .01,
  transmission_rate = .02,
  recovery_rate = 1 / 7
)

agents_smallworld(
  model_sir,
  n = 2000,
  k = 10,
  d = FALSE,
  p = 0.01
)

## ----run-sir------------------------------------------------------------------
verbose_off(model_sir)

run(
  model_sir,
  ndays = 50,
  seed = model_seed
)

summary(model_sir)

## ----get-sir-model-data-------------------------------------------------------
model_sir_data <- get_today_total(model_sir)

## ----simfun-------------------------------------------------------------------
simulation_fun <- function(params, lfmcmc_obj) {

  set_param(model_sir, "Recovery rate", params[1])
  set_param(model_sir, "Transmission rate", params[2])

  run(
    model_sir,
    ndays = 50
  )

  get_today_total(model_sir)

}

## ----sumfun-------------------------------------------------------------------
summary_fun <- function(data, lfmcmc_obj) {
  return(data)
}

## ----propfun------------------------------------------------------------------
proposal_fun <- function(old_params, lfmcmc_obj) {
  res <- plogis(qlogis(old_params) + rnorm(length(old_params), sd = .1))
  return(res)
}

## ----kernfun------------------------------------------------------------------
kernel_fun <- function(
  simulated_stats, observed_stats, epsilon, lfmcmc_obj
) {

  diff <- ((simulated_stats - observed_stats)^2)^epsilon
  dnorm(sqrt(sum(diff)))

}

## ----lfmcmc-setup-------------------------------------------------------------
lfmcmc_model <- LFMCMC(model_sir) |>
  set_simulation_fun(simulation_fun) |>
  set_summary_fun(summary_fun) |>
  set_proposal_fun(proposal_fun) |>
  set_kernel_fun(kernel_fun) |>
  set_observed_data(model_sir_data)

## ----lfmcmc-run---------------------------------------------------------------
initial_params <- c(0.3, 0.3)
epsilon <- 1.0
n_samples <- 2000

# Run the LFMCMC simulation
run_lfmcmc(
  lfmcmc = lfmcmc_model,
  params_init = initial_params,
  n_samples = n_samples,
  epsilon = epsilon,
  seed = model_seed
)

## ----lfmcmc-print-------------------------------------------------------------
set_params_names(lfmcmc_model, c("Recovery rate", "Transmission rate"))
set_stats_names(lfmcmc_model, get_states(model_sir))

print(lfmcmc_model, burnin = 1500)

## ----post-dist----------------------------------------------------------------
# Extracting the accepted parameters
accepted <- get_all_accepted_params(lfmcmc_model)

# Plotting the trace
plot(
  accepted[, 1], type = "l", ylim = c(0, 1),
  main = "Trace of the parameters",
  lwd = 2,
  col = "tomato",
  xlab = "Step",
  ylab = "Parameter value"
)

lines(accepted[, 2], type = "l", lwd = 2, col = "steelblue")

legend(
  "topright",
  bty = "n",
  legend = c("Recovery rate", "Transmission rate"),
  pch    = 20,
  col    = c("tomato", "steelblue")
)

