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
        initialize = function() {},

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
            start_date = ymd('2020-02-24'),
            end_date = Sys.Date() - 1
        ) {
            private$tab <-
                lapply(
                    seq(start_date, end_date, by = '1 day'),
                    function(x) {
                        tbl_input <-
                            try(file.path(
                                private$repo,
                                glue::glue(
                                    'dpc-covid19-ita-andamento-nazionale-{format(x, "%Y%m%d")}.csv'
                                )
                            ) %>%
                                readr::read_csv(col_types = readr::cols()),
                            silent = TRUE
                            )
                        if (class(tbl_input)[1] != 'try-error') {
                            return(tbl_input)
                        } else {
                            closeAllConnections()
                            return()
                        }
                    }
                ) %>%
                dplyr::bind_rows() %>%
                dplyr::mutate(data = as_date(data))
            invisible(self)
        },

        #' @description
        #' This plots a barchart of the summary.
        plot = function() {
            if (is.null(private$tab))
                stop('Nothing to plot.\n')
            highchart() %>%
                hc_chart(type = 'column') %>%
                hc_xAxis(categories = private$tab$data) %>%
                hc_yAxis(title = list(text = 'n')) %>%
                hc_plotOptions(series = list(stacking = 'normal')) %>%
                hc_legend(itemStyle = list(fontSize = '10px')) %>%

                # Deceduti
                hc_add_series(
                    data = tbl$deceduti,
                    name = 'Deaths'
                ) %>%

                # Terapia intensiva
                hc_add_series(
                    data = tbl$terapia_intensiva,
                    name = 'Intensive Care'
                ) %>%

                # Ricoverati con sintomi
                hc_add_series(
                    data = tbl$totale_casi,
                    name = 'Hospitalized (no IC)'
                ) %>%

                # Isolamento domiciliare
                hc_add_series(
                    data = tbl$isolamento_domiciliare,
                    name = 'Home Isolation'
                ) %>%

                # Dimessi guariti
                hc_add_series(
                    data = tbl$dimessi_guariti,
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
