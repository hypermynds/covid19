#' R6 Covid-19 Class
#'
#' @description
#' This object type is used to retrieve, analyse and plot international data on
#' COVID19 outbreak and spreading.
#'
#' @param region is a string; the name of the region to be returned.
#' Use "ITA" to return the national aggregated data.
#' @param k is an integer; is the width of the historical window used
#' to estimate the regression parameters.
#' @param log is a boolean; if \code{TRUE}, a logarithmic scale is applied to y
#' axis in plots.
#'
#' @import highcharter
#' @export
Covid <- R6::R6Class(

    # Class Covid ----
    classname = 'Covid',

    # * Public Members ----
    public = list(

        #' @description
        #' Create a new Covid object.
        initialize = function() {
            invisible(self)
        },

        #' @description
        #' This is to print the object type.
        print = function() {
            cat('Covid19 Class Object.\n')
        },

        #' @description
        #' This returns the table with raw data.
        get = function(region = NULL) {
            if (is.null(private$tab)) self$update()
            if (is.null(region)) {
                private$tab
            } else if (region == 'ITA') {
                private$tab %>%
                    dplyr::select(-denominazione_regione) %>%
                    dplyr::group_by(data) %>%
                    dplyr::summarise_all(sum)
            } else {
                private$tab %>%
                    dplyr::filter(denominazione_regione == region) %>%
                    dplyr::select(-denominazione_regione)
            }
        },

        #' @description
        #' The list of all available regions.
        regions = function() {
            lst_regions <-
                self$get() %>%
                dplyr::distinct(denominazione_regione) %>%
                dplyr::pull()
            names(lst_regions) <- lst_regions
            lst_regions
        },

        #' @description
        #' This updates the inner table downloading data from the remote repo.
        update = function() {
            private$tab <-
                private$repo %>%
                readr::read_csv(col_types = readr::cols()) %>%
                dplyr::mutate(
                    data = lubridate::as_date(data),
                    denominazione_regione =
                        dplyr::case_when(
                            codice_regione == '04' ~ 'Trentino-Alto Adige',
                            codice_regione == '06' ~ 'Friuli-Venezia Giulia',
                            codice_regione == '08' ~ 'Emilia-Romagna',
                            TRUE ~ denominazione_regione
                        )
                ) %>%
                dplyr::select(-stato, -codice_regione, -lat, -long) %>%
                dplyr::group_by(data, denominazione_regione) %>%
                dplyr::summarise_all(sum) %>%
                dplyr::ungroup()
            invisible(self)
        },

        #' @description
        #' This is to predict the behavior on the following days.
        #' @param n is an integer; the number of days to forecast.
        forecast = function(region = 'ITA', n = 5L, k = 10L) {
            # Predict the total number of cases
            fit1 <-
                lm(
                    formula = log(totale_casi) ~ data,
                    data = self$get(region) %>%
                        dplyr::filter(totale_casi != 0) %>%
                        tail(k)
                )
            # Predict the total number of deaths
            fit2 <- lm(
                formula = log(deceduti) ~ data,
                data = self$get(region) %>%
                    dplyr::filter(deceduti != 0) %>%
                    tail(k)
            )
            # Forecast on the following n days
            new_dates <-
                seq(
                    max(private$tab$data) + 1,
                    by = '1 day',
                    length.out = n
                )
            self$get(region) %>%
                dplyr::select(data, totale_casi, deceduti) %>%
                dplyr::bind_rows(
                    dplyr::tibble(
                        data = new_dates,
                        totale_casi = exp(predict(fit1, new_dates)),
                        deceduti = exp(predict(fit2, new_dates))
                    ) %>%
                        dplyr::mutate_if(is.double, as.integer)
                )
        },

        #' @description
        #' This is used to estimate the growing factor.
        grow_rate = function(region = 'ITA', k = 10L) {
            lm(
                formula = log(totale_casi) ~ data,
                data = self$get(region) %>% tail(k)
            ) %>%
                magrittr::use_series(coefficients) %>%
                magrittr::extract2('data') %>%
                magrittr::multiply_by(100)
        },

        #' @description
        #' This is used to export a plottable table
        table = function(region = 'ITA') {
            self$get(region) %>%
                dplyr::mutate(
                    data = format(data, '%d/%m/%Y')
                ) %>%
                dplyr::mutate_if(is.double, as.integer) %>%
                dplyr::mutate_if(is.integer, function(x) format(x, big.mark = '.', decimal.mark = ',')) %>%
                set_colnames(simple_cap(gsub('_', ' ', names(.))))
        },

        #' @description
        #' This plots a barchart of the summary.
        plot_hist = function(region = 'ITA', log = FALSE) {
            tbl_hist <- self$get(region)
            hc <-
                highchart() %>%
                hc_chart(type = 'column') %>%
                hc_xAxis(
                    categories = tbl_hist$data %>%
                        format('%e %b')
                ) %>%
                hc_plotOptions(series = list(stacking = 'normal'))
            if (log) {
                hc %<>%
                    hc_yAxis(
                        type = 'logarithmic'
                    )
            }
            hc %<>%
                # Deceduti
                hc_add_series(
                    data = tbl_hist$deceduti,
                    name = 'Deaths'
                ) %>%
                # Terapia intensiva
                hc_add_series(
                    data = tbl_hist$terapia_intensiva,
                    name = 'Intensive Care'
                ) %>%
                # Ricoverati con sintomi
                hc_add_series(
                    data = tbl_hist$ricoverati_con_sintomi,
                    name = 'Hospitalized (no IC)'
                ) %>%
                # Isolamento domiciliare
                hc_add_series(
                    data = tbl_hist$isolamento_domiciliare,
                    name = 'Home Isolation'
                ) %>%
                # Dimessi guariti
                hc_add_series(
                    data = tbl_hist$dimessi_guariti,
                    name = 'Recovered'
                )
            hc
        },

        #' @description
        #' This plots a linechart of the forecast.
        plot_fore = function(region = 'ITA', log = FALSE) {
            tbl_forecast <- self$forecast(region)
            hc <-
                highchart() %>%
                hc_xAxis(
                    categories = tbl_forecast$data %>%
                        format('%e %b')
                )
            if (log) {
                hc %<>%
                    hc_yAxis(
                        type = 'logarithmic'
                    )
            }
            hc %<>%
                # Deceduti
                hc_add_series(
                    data = tbl_forecast$deceduti,
                    name = 'Deaths'
                ) %>%
                # Terapia intensiva
                hc_add_series(
                    data = tbl_forecast$totale_casi,
                    name = 'Total Cases'
                )
            hc
        }

    ),

    # * Private Members ----
    private = list(

        tab = NULL,
        repo = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv'

    )

)
