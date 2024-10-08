## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", out.width = "80%", fig.width = 7, fig.height = 5,
  fig.align = "center"
)

## ----entity-matrix-setup------------------------------------------------------
library(epiworldR)

e1 <- entity("Population 1", 3e3, as_proportion = FALSE)
e2 <- entity("Population 2", 3e3, as_proportion = FALSE)
e3 <- entity("Population 3", 3e3, as_proportion = FALSE)

# Row-stochastic matrix (rowsums 1)
cmatrix <- c(
  c(0.9, 0.05, 0.05),
  c(0.1, 0.8, 0.1),
  c(0.1, 0.2, 0.7)
) |> matrix(byrow = TRUE, nrow = 3)

## ----model-build--------------------------------------------------------------
N <- 9e3

flu_model <- ModelSEIRMixing(
  name              = "Flu",
  n                 = N,
  prevalence        = 1 / N,
  contact_rate      = 20,
  transmission_rate = 0.1,
  recovery_rate     = 1 / 7,
  incubation_days   = 7,
  contact_matrix    = cmatrix
)

# Adding the entities
flu_model |>
  add_entity(e1) |>
  add_entity(e2) |>
  add_entity(e3)

## ----model-simulate-----------------------------------------------------------
set.seed(331)
run(flu_model, ndays = 100)
summary(flu_model)
plot_incidence(flu_model)

## ----investigate, eval=TRUE---------------------------------------------------
library(data.table)

agents_entities <- lapply(get_entities(flu_model), \(e) {
  entity_get_agents(e)
}) |> rbindlist()

head(agents_entities)

## ----transmissions------------------------------------------------------------
# Retrieving the transmissions
transmissions <- get_transmissions(flu_model) |>
  data.table()

# We only need the date and the source
transmissions <- subset(
  transmissions,
  select = c("date", "source")
)

# Attaching the entity to the source
transmissions <- merge(
  transmissions,
  agents_entities,
  by.x = "source", by.y = "agent"
)

# Aggregating by date x entity (counts)
transmissions <- transmissions[, .N, by = .(date, entity)]

# Taking a look at the data
head(transmissions)

## ----transmissions-plot-------------------------------------------------------
setorder(transmissions, date, entity)

ran <- range(transmissions$N)
transmissions[entity == 0, plot(
  x = date, y = N, type = "l", col = "black", ylim = ran)]
transmissions[entity == 1, lines(x = date, y = N, col = "red")]
transmissions[entity == 2, lines(x = date, y = N, col = "blue")]

legend(
  "topright",
  legend = c("Population 1", "Population 2", "Population 3"),
  col = c("black", "red", "blue"),
  lty = 1
)

