# Stochastic SEIR Model in R

# Load required packages
install.packages(c("EpiModel", "deSolve", "pomp"))
library(EpiModel)   # For agent-based modeling
library(deSolve)    # For stochastic differential equations
library(pomp)       # Alternative for SDEs with particle filtering

#------------------------------------------------------------#
# 1. Agent-Based SEIR Model using EpiModel
#------------------------------------------------------------#

# Define population size and initial conditions
num_individuals <- 1000
init <- c(S = 990, E = 5, I = 5, R = 0) # Initial counts

# Define parameters
params <- list(
  beta = 0.3,   # Transmission rate
  sigma = 1/8,  # Incubation rate (1/latent period)
  gamma = 1/7   # Recovery rate (1/infectious period)
)

# Define the transition matrix for SEIR
seir_transitions <- function(t, state, params) {
  with(as.list(c(state, params)), {
    N <- S + E + I + R
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}

# Run the agent-based model using EpiModel
seir_model <- EpiModel(
  type = "SEIR",
  param = params,
  init = init,
  control = control.icm(type = "SIR", nsteps = 100)
)

plot(seir_model)

#------------------------------------------------------------#
# 2. Stochastic SEIR Model using deSolve (SDE)
#------------------------------------------------------------#

seir_sde <- function(time, state, params) {
  with(as.list(c(state, params)), {
    N <- S + E + I + R
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - sigma * E
    dI <- sigma * E - gamma * I
    dR <- gamma * I
    return(list(c(dS, dE, dI, dR)))
  })
}

# Define initial values for the SDE model
state <- c(S = 990, E = 5, I = 5, R = 0)
time <- seq(0, 100, by = 1)

# Solve the SDE using deSolve
seir_out <- ode(y = state, times = time, func = seir_sde, parms = params, method = "rk4")

# Plot the results
matplot(seir_out[, 1], seir_out[, -1], type = "l", lty = 1, col = c("blue", "orange", "red", "green"),
        xlab = "Time", ylab = "Population", main = "Stochastic SEIR Model")
legend("right", legend = c("S", "E", "I", "R"), col = c("blue", "orange", "red", "green"), lty = 1)

#------------------------------------------------------------#
# Next Steps:
# - Add stochasticity using the Gillespie algorithm (for more randomness)
# - Incorporate real data for parameter estimation
# - Compare results of ABM vs. SDE
#------------------------------------------------------------#
