# A Time-dependent SIR model for COVID-19
# By Chen-Lu-Chang

# X(t) and R(t) are, respectively, the number of infected and recovered people
# for each t >= 0.

# Libraries
require(dplyr)
# require(ridge)
require(tidyr)
# require(glmnet)

# Fit the model
tbl_data <-
    Covid$new()$get('ITA') %>%
    mutate(R = deceduti + dimessi_guariti) %>%
    select(data, X = totale_attualmente_positivi, R, D = deceduti)

# The fit will go ahead till the end of June
tbl_data %<>%
    bind_rows(
        tibble(
            data = seq(max(tbl_data$data) + 1, as.Date('2020-03-31'), by = '1 day'),
            X = as.numeric(NA),
            R = as.numeric(NA),
            D = as.numeric(NA)
        )
    )

# Let's cycle to find the missing values
idx <- which(is.na(tbl_data$X))
for (i in idx) {

    # 1. Historical beta(t) and gamma(t) ----
    tbl_data %<>%
        mutate(
            X_1 = lead(X),
            R_1 = lead(R),
            D_1 = lead(D),
            beta = ((X_1 - X) + (R_1 - R)) / X,
            gamma = (R_1 - R) / X,
            rho = (D_1 - D) / R
        )

    # # 2. Ridge Regression ----
    #
    # # Beta
    # tbl_rr <-
    #     tbl_data %>%
    #     mutate(
    #         beta_1 = lag(beta),
    #         beta_2 = lag(beta, 2),
    #         beta_3 = lag(beta, 3)
    #     ) %>%
    #     select(starts_with('beta')) %>%
    #     drop_na()
    # x <- as.matrix(tbl_rr[, 2:4])
    # y <- tbl_rr$beta
    # lambda <- 10^seq(10, -2, length = 100)
    # ridge.mod.beta <- glmnet(x, y, alpha = 0, lambda = lambda)
    # cv.out.beta <- suppressWarnings(cv.glmnet(x, y, alpha = 0))
    #
    # # Gamma
    # tbl_rr <-
    #     tbl_data %>%
    #     mutate(
    #         gamma_1 = lag(gamma),
    #         gamma_2 = lag(gamma, 2),
    #         gamma_3 = lag(gamma, 3)
    #     ) %>%
    #     select(starts_with('gamma')) %>%
    #     drop_na()
    # x <- as.matrix(tbl_rr[, 2:4])
    # y <- tbl_rr$gamma
    # lambda <- 10^seq(10, -2, length = 100)
    # ridge.mod.gamma <- glmnet(x, y, alpha = 0, lambda = lambda)
    # cv.out.gamma <- suppressWarnings(cv.glmnet(x, y, alpha = 0))
    #
    # # 3. Find beta(t) and gamma(t) ----
    #
    # # Beta
    # tbl_new <-
    #     tbl_data %>%
    #     mutate(
    #         beta_1 = lag(beta),
    #         beta_2 = lag(beta, 2),
    #         beta_3 = lag(beta, 3)
    #     ) %>%
    #     select(starts_with('beta_')) %>%
    #     drop_na() %>%
    #     tail(1)
    # x <- as.matrix(tbl_new)
    # tbl_data$beta[i - 1] <- predict(ridge.mod.beta, s = cv.out.beta$lambda.min, newx = x)
    #
    # # Gamma
    # tbl_new <-
    #     tbl_data %>%
    #     mutate(
    #         gamma_1 = lag(gamma),
    #         gamma_2 = lag(gamma, 2),
    #         gamma_3 = lag(gamma, 3)
    #     ) %>%
    #     select(starts_with('gamma_')) %>%
    #     drop_na() %>%
    #     tail(1)
    # x <- as.matrix(tbl_new)
    # tbl_data$gamma[i - 1] <- predict(ridge.mod.gamma, s = cv.out.gamma$lambda.min, newx = x)

    # 3. Standard regression ----
    fit_beta <- lm(log(beta) ~ data, data = tbl_data %>% drop_na())
    tbl_data$beta[i - 1] <- predict(fit_beta, newdata = tbl_data$data[i - 1]) %>% exp()
    fit_gamma <- lm(log(gamma + 1) ~ data, data = tbl_data %>% drop_na())
    tbl_data$gamma[i - 1] <- exp(predict(fit_gamma, newdata = tbl_data$data[i - 1])) - 1

    # 4. Compute X(t) and R(t) ----
    tbl_data$X[i] <- (1 + tbl_data$beta[i - 1] - tbl_data$gamma[i - 1]) * tbl_data$X[i - 1]
    tbl_data$R[i] <- tbl_data$R[i - 1] + tbl_data$gamma[i - 1] * tbl_data$X[i - 1]

}

# Plot results
tbl_data %>%
    with({
        plot(data, X + R + D, type = 'b', col = 'green', ylab = '');
        lines(data, X, type = 'b', col = 'red');
        lines(data, R, type = 'b', col = 'blue');
        lines(data, D, type = 'b', col = 'maroon');
        abline(v = Sys.Date(), lty = 3)
    })
tbl_data %>%
    dplyr::filter(data <= Sys.Date()) %>%
    with({
        plot(data, beta, type = 'b', col = 'red', ylab = '', ylim = c(0, 0.8));
        lines(data, gamma, type = 'b', col = 'blue');
        lines(data, rho, type = 'b', col = 'green');
        abline(v = Sys.Date(), lty = 3)
    })

# Forecast for tomorrow
tbl_data %>%
    mutate(
        R0 = beta / gamma,
        total = X + R,
        increment = c(0, diff(total))
    ) %>%
    filter(data >= '2020-03-01') %>%
    print(n = 61)
