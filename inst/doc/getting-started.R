## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "80%", fig.width = 7, fig.height = 5, 
  fig.align = "center"
)

## ----sirconn-setup------------------------------------------------------------
library(epiworldR)
model_sir <- ModelSIRCONN(
  name              = "COVID-19",
  n                 = 100000, 
  prevalence        = 0.0001, 
  contact_rate      = 2,
  prob_transmission = 0.5,
  prob_recovery     = 1/3
  )

## -----------------------------------------------------------------------------
model_sir

## -----------------------------------------------------------------------------
run(model_sir, ndays = 50, seed = 1912)
model_sir

## ----showing-methods----------------------------------------------------------
methods(class = "epiworld_model")

## -----------------------------------------------------------------------------
plot(model_sir)

## ----get-hist-total-----------------------------------------------------------
head(get_hist_total(model_sir))

## ----repnum-------------------------------------------------------------------
repnum <- get_reproductive_number(model_sir)
head(repnum)

## -----------------------------------------------------------------------------
x <- plot(repnum, type = "b")

## -----------------------------------------------------------------------------
plot_incidence(model_sir)

## ----design-and-add-----------------------------------------------------------
# Building the virus
flu <- virus(name = "Flu", prob_infecting = .3)

# Adding the virus to the model
add_virus(model_sir, flu, .0001)

## -----------------------------------------------------------------------------
run(model_sir, ndays = 50, seed = 1912)
model_sir

## ---- fig.height=10-----------------------------------------------------------
repnum2 <- get_reproductive_number(model_sir)

op <- par(mfrow = c(2,1))
plot(model_sir)
plot(repnum2, type="b")
par(op)

## -----------------------------------------------------------------------------
# Removing the flu virus from the model
rm_virus(model_sir, 1)

vaccine <- tool(
  name = "Vaccine",
  susceptibility_reduction = .9,
  transmission_reduction = .5,
  recovery_enhancer = .5, 
  death_reduction = .9
)

add_tool(model_sir, vaccine, 0.5)
run(model_sir, ndays = 50, seed = 1231)

## ----curves-including-vaccine, fig.height=10----------------------------------
repnum3 <- get_reproductive_number(model_sir)

op <- par(mfrow = c(2,1))
plot(model_sir)
plot(repnum3, type="b")
par(op)

