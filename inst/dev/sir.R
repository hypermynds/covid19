require(deSolve)
require(dplyr)
require(lubridate)

# Modello SIR ----

# Cerco di calcolare il fattore R0 in Lombardia
tbl_data <-
    Covid$new()$get('Lombardia') %>%
    mutate(
        cum_recovered = dimessi_guariti + deceduti
    ) %>%
    select(
        data,
        cum_incidence = totale_attualmente_positivi,
        cum_recovered
    )

# Considero la popolazione totale come potenzialmente contagiabile
# ma, per effetto delle misure prese, considero solo una quota
N <- 10e6 * 0.5

# Definisco il modello differenziale
# SIR <- function(time, state, parameters) {
#     par <- as.list(c(state, parameters))
#     with(par, {
#         dS <- -beta * I * S/N
#         dI <- beta * I * S/N - gamma * I
#         dR <- gamma * I
#         list(c(dS, dI, dR))
#     })
# }
SEIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta * S * I / N
        dE <- beta * S * I / N - kappa * E
        dI <- kappa * E - gamma * I
        dR <- gamma * I
        list(c(dS, dE, dI, dR))
    })
}
# SEIR <- function(time, state, parameters) {
#     par <- as.list(c(state, parameters))
#     with(par, {
#         dS <- -beta * S * (I + epsilon * E) / N
#         dE <- beta * S * (I + epsilon * E) / N - kappa * E
#         dI <- kappa * E - gamma * I
#         dR <- gamma * I
#         list(c(dS, dE, dI, dR))
#     })
# }

# Definisco i dati iniziali
# init <- c(S = N - tbl_data$cum_incidence[1], I = tbl_data$cum_incidence[1], R = 0)
init <- c(
    S = N - 2 * tbl_data$cum_incidence[1],
    E = tbl_data$cum_incidence[1],
    I = tbl_data$cum_incidence[1],
    R = tbl_data$cum_recovered[1]
)

# Fitto il modello
# RSS <- function(parameters) {
#     names(parameters) <- c("beta", "gamma")
#     out <- ode(y = init, times = 1:nrow(tbl_data), func = SIR, parms = parameters)
#     fit1 <- out[, 3]
#     fit2 <- out[, 4]
#     sum((tbl_data$cum_incidence - fit1)^2 + (tbl_data$cum_recovered - fit2)^2)
# }
RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma", "kappa")
    out <- ode(y = init, times = 1:nrow(tbl_data), func = SEIR, parms = parameters)
    fit1 <- out[, 4]
    fit2 <- out[, 5]
    sum((tbl_data$cum_incidence - fit1)^2 + (tbl_data$cum_recovered - fit2)^2)
}

# Ottimizzo i parametri
Opt <- optim(
    par = rep(0.5, 3),
    fn = RSS,
    method = "L-BFGS-B",
    lower = rep(0, 3),
    upper = rep(1, 3)
)
Opt$message

# Definisco i parametri
Opt_par <- setNames(Opt$par, c("beta", "gamma", "kappa"))
Opt_par

# Calcolo il valore di R0
# R0 <- Opt_par[1] / Opt_par[2]
R0 <- Opt_par[1] / Opt_par[2]
R0

# Plotto i fitted vs reali
tbl_fit <-
    as_tibble(
        ode(y = init, times = 1:nrow(tbl_data),
            func = SEIR, parms = Opt_par)
    ) %>%
    bind_cols(
        data = tbl_data$data,
        I_obs = tbl_data$cum_incidence,
        R_obs = tbl_data$cum_recovered
    )
tbl_fit %>%
    with({
        plot(data, I, type = 'l', col = 'red');
        lines(data, I_obs, type = 'b', col = 'green')
    })

# Plotto 4 mesi consecutivi
n_days <- 120
tbl_fit <-
    as_tibble(
        ode(y = init, times = 1:n_days,
            func = SEIR, parms = Opt_par)
    ) %>%
    mutate(data = seq(ymd('2020-02-24'), by = '1 day', length.out = n_days)) %>%
    left_join(
        tbl_data,
        by = 'data'
    ) %>%
    rename(I_obs = cum_incidence, R_obs = cum_recovered)
tbl_fit %>%
    with({
        plot(data, I, type = 'l', col = 'red', ylim = c(0, N));
        lines(data, I_obs, type = 'b', col = 'green');
        lines(data, R, col = 'blue');
        lines(data, R_obs, type = 'b', col = 'yellow')
    })
