## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "80%", fig.width = 7, fig.height = 5,
  fig.align = "center"
)

## ----create-base-model--------------------------------------------------------
library(epiworldR)

model_sirconn <- ModelSIRCONN(
  name              = "COVID-19",
  n                 = 50000,
  contact_rate      = 4,
  recovery_rate     = 1 / 4,
  prevalence        = 0.001,
  transmission_rate = 0.5
)

## ----run-base-model-----------------------------------------------------------
verbose_off(model_sirconn)
run(model_sirconn, ndays = 50, seed = 1912)
plot(model_sirconn)

## ----create-flu-virus---------------------------------------------------------
flu_virus <- virus(name = "Flu", prob_infecting = .35, prevalence = 0.001, as_proportion = TRUE)

## ----add-flu------------------------------------------------------------------
add_virus(model_sirconn, flu_virus)

## ----run-model-flu------------------------------------------------------------
run(model_sirconn, ndays = 50, seed = 1912)
plot(model_sirconn)

## ----create-vaccine-----------------------------------------------------------
vaccine_tool <- tool(
  name = "Vaccine",
  susceptibility_reduction = .9,
  transmission_reduction = .5,
  recovery_enhancer = .5,
  death_reduction = .9,
  prevalence = 0.5,
  as_proportion = TRUE
)

## ----set-vaccine-distribution-------------------------------------------------
set_distribution_tool(
  tool = vaccine_tool,
  distfun = distribute_tool_randomly(0.5, TRUE)
)

## ----add-vaccine--------------------------------------------------------------
add_tool(model_sirconn, vaccine_tool)

## ----run-model-vaccine--------------------------------------------------------
run(model_sirconn, ndays = 50, seed = 1912)
plot(model_sirconn)

## ----set-events---------------------------------------------------------------
isolation_day_10 <- globalevent_set_params("Contact rate", 2, day = 10)
advertisement_day_20 <- globalevent_set_params("Contact rate", 1.5, day = 20)

## ----add-events---------------------------------------------------------------
add_globalevent(model_sirconn, isolation_day_10)
add_globalevent(model_sirconn, advertisement_day_20)

## ----run-full-model-----------------------------------------------------------
run(model_sirconn, ndays = 50, seed = 1912)
plot(model_sirconn)

## ----model-summary------------------------------------------------------------
summary(model_sirconn)

## ----reproductive-numbers-----------------------------------------------------
repnum2 <- get_reproductive_number(model_sirconn)
plot(repnum2, type = "b")

