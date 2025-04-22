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
  n                 = 50000,
  prevalence        = 0.0001,
  contact_rate      = 2,
  transmission_rate = 0.5,
  recovery_rate     = 1 / 3
)

# Printing the model
model_sir

## ----summary-method-----------------------------------------------------------
summary(model_sir)

## -----------------------------------------------------------------------------
run(model_sir, ndays = 50, seed = 1912)
summary(model_sir)

## ----getting-totals, echo=FALSE-----------------------------------------------
initials <- get_hist_total(model_sir)[1:3, ]$counts |> prettyNum(big.mark = ",")
finals   <- get_today_total(model_sir) |> prettyNum(big.mark = ",")
tmat     <- get_transition_probability(model_sir)
tmat     <- round(tmat, digits = 2)

## ----mermaid-diagram----------------------------------------------------------
library(DiagrammeR)
# Capture the output of the draw_mermaid function
m_diagram <- draw_mermaid(model_sir)
# Modify first line for compatibility with DiagrammeR
# - Necessary because DiagrammeR uses old version of mermaid.js
m_diagram <- paste0("graph", substring(m_diagram, 10))

mermaid(m_diagram)

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
subset(x, date == 10) # Reproductive number on day 10

## -----------------------------------------------------------------------------
plot_incidence(model_sir)

## ----design-and-add-----------------------------------------------------------
# Building the virus
flu <- virus(
  name = "Flu", prob_infecting = .3,
  prevalence = .0001, as_proportion = TRUE
)

# Adding the virus to the model
add_virus(model_sir, flu)

## -----------------------------------------------------------------------------
run(model_sir, ndays = 50, seed = 1912)
model_sir

## ----fig.height=10------------------------------------------------------------
repnum2 <- get_reproductive_number(model_sir)

op <- par(mfrow = c(2, 1))
plot(model_sir)
plot(repnum2, type = "b")
par(op)

## -----------------------------------------------------------------------------
# Removing the flu virus from the model
rm_virus(model_sir, 1)

vaccine <- tool(
  name = "Vaccine",
  prevalence = 0.5,
  as_proportion = TRUE,
  susceptibility_reduction = .9,
  transmission_reduction = .5,
  recovery_enhancer = .5,
  death_reduction = .9
)

add_tool(model_sir, vaccine)
run(model_sir, ndays = 50, seed = 1231)

## ----curves-including-vaccine, fig.height=10----------------------------------
repnum3 <- get_reproductive_number(model_sir)

op <- par(mfrow = c(2, 1))
plot_incidence(model_sir)
plot(repnum3, type = "b")
par(op)

