require(covid19)
require(dplyr)
require(lubridate)

covid <- Covid$new()
value_dates <- seq(as_date('2020-03-05'), Sys.Date() - 1, by = '1 day')

tbl_pred <-
    lapply(
        value_dates,
        function(x) {
            covid$sir(fit_date = x - 1, end_date = Sys.Date()) %>%
                slice(which.max(X)) %>%
                mutate(value_date = x) %>%
                select(value_date, peak_date = data, peak_value = X)
        }
    ) %>%
    bind_rows()

# Previsione data
tbl_pred %>%
    filter(value_date >= '2020-04-01') %>%
    with(plot(value_date, peak_date, type = 'b'))
abline(h = as.Date('2020-04-19'), col = 'red')

# Previsione valore
tbl_pred %>%
    filter(value_date >= '2020-04-01') %>%
    with(plot(value_date, peak_value, type = 'b'))
abline(h = 108257, col = 'red')
