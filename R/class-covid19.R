#' R6 Covid-19 Class
#'
#' @description
#' This object type is used to retrieve, analyse and plot international data on
#' COVID19 outbreak and spreading.
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
        plot = function() {
            if (is.null(private$tab))
                stop('Nothing to plot.\n')
            highchart() %>%
                hc_chart(type = 'column') %>%
                hc_xAxis(
                    categories = private$tab$data %>%
                        format('%e %b')
                ) %>%
                hc_plotOptions(series = list(stacking = 'normal')) %>%
                hc_legend(itemStyle = list(fontSize = '10px')) %>%

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

        }

    ),

    # * Private Members ----
    private = list(

        tab = NULL,

        repo = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale'

    )

)
