#' R6 Covid-19 Class
#'
#' @description
#' This object type is used to retrieve, analyse and plot international data on
#' COVID19 outbreak and spreading.
#'
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
        get = function() {
            if (is.null(private$tab)) self$update()
            private$tab
        },

        #' @description
        #' This updates the inner table downloading data from the remote repo.
        #' @param start_date is a date; the beginning of the update period.
        #' @param end_date is a date; the end of the update period.
        update = function(
            start_date = lubridate::ymd('2020-02-24'),
            end_date = Sys.Date()
        ) {
            private$tab <-
                lapply(
                    seq(start_date, end_date, by = '1 day'),
                    function(x) {
                        raw_data <-
                            file.path(
                                private$repo,
                                glue::glue(
                                    'dpc-covid19-ita-andamento-nazionale-{format(x, "%Y%m%d")}.csv'
                                )
                            ) %>%
                                httr::GET()
                        if (raw_data$status_code != 200) {
                            return()
                        } else {
                            raw_data %>%
                                httr::content() %>%
                                readr::read_csv()
                        }
                    }
                ) %>%
                dplyr::bind_rows() %>%
                dplyr::mutate(data = lubridate::as_date(data))
            invisible(self)
        },

        #' @description
        #' This is to predict the behavior on the following days.
        #' @param n is an integer; the number of days to forecast.
        forecast = function(n = 5L) {
            if (is.null(private$tab))
                stop('Nothing to forecast.\n')
            # Predict the total number of cases
            fit1 <- lm(log(totale_casi) ~ data, data = private$tab)
            # Predict the total number of deaths
            fit2 <- lm(log(deceduti) ~ data, data = private$tab)
            # Forecast on the following n days
            new_dates <-
                seq(
                    max(private$tab$data) + 1,
                    by = '1 day',
                    length.out = n
                )
            private$tab %>%
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
        #' This is used to export a plottable table
        table = function() {
            if (is.null(private$tab))
                stop('Nothing to tab.\n')
            private$tab %>%
                dplyr::mutate(
                    data = format(data, '%d/%m/%Y')
                ) %>%
                dplyr::mutate_if(is.double, as.integer) %>%
                dplyr::mutate_if(is.integer, function(x) format(x, big.mark = '.', decimal.mark = ',')) %>%
                set_colnames(simple_cap(gsub('_', ' ', names(.))))
        },

        #' @description
        #' This plots a barchart of the summary.
        plot_hist = function(log = FALSE) {
            if (is.null(private$tab))
                stop('Nothing to plot.\n')
            hc <-
                highchart() %>%
                hc_chart(type = 'column') %>%
                hc_xAxis(
                    categories = private$tab$data %>%
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
                    data = private$tab$deceduti,
                    name = 'Deaths'
                ) %>%
                # Terapia intensiva
                hc_add_series(
                    data = private$tab$terapia_intensiva,
                    name = 'Intensive Care'
                ) %>%
                # Ricoverati con sintomi
                hc_add_series(
                    data = private$tab$ricoverati_con_sintomi,
                    name = 'Hospitalized (no IC)'
                ) %>%
                # Isolamento domiciliare
                hc_add_series(
                    data = private$tab$isolamento_domiciliare,
                    name = 'Home Isolation'
                ) %>%
                # Dimessi guariti
                hc_add_series(
                    data = private$tab$dimessi_guariti,
                    name = 'Recovered'
                )
            hc
        },

        #' @description
        #' This plots a linechart of the forecast.
        plot_fore = function(log = FALSE) {
            if (is.null(private$tab))
                stop('Nothing to plot.\n')
            tbl_forecast <- self$forecast()
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

        repo = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale'

    )

)
