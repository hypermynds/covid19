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
                filter(data == x) %>%
                select(data, total)
        }
    ) %>%
    bind_rows() %>%
    select(data, forecast = total)

tbl_actual <-
    covid$get(region = 'ITA') %>%
    filter(data %in% value_dates) %>%
    select(data, actual = totale_casi)

tbl_output <-
    tbl_pred %>%
    left_join(tbl_actual, by = 'data') %>%
    mutate(
        delta = actual - forecast,
        error = delta / forecast
    )

tbl_output %>%
    with({
        plot(data, abs(error), type = 'b');
        abline(h = 3/100, lty = 3, col = 'blue')
    })

tbl_output %>%
    summarise(100 * mean(abs(error))) %>%
    pull()
