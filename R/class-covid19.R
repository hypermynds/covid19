#' R6 Covid-19 Class
#'
#' @description
#' This object type is used to retrieve, analyse and plot international data on
#' COVID19 outbreak and spreading.
#'
#' @param region is a string; the name of the region to be returned.
#' Use "ITA" to return the national aggregated data.
#' @param series is a string; the name of the field that has to be considered.
#' @param n is an integer; the number of days to forecast.
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
        forecast = function(region = 'ITA', series = 'totale_casi', n = 5L, k = 10L) {
            fit <-
                try(
                    lm(
                        formula = log(get(series)) ~ data,
                        data = self$get(region) %>%
                            dplyr::filter(totale_casi != 0) %>%
                            tail(k)
                    ),
                    silent = TRUE
                )
            if (class(fit)[1] == 'try-error')
                return()
            new_dates <-
                seq(
                    max(self$get(region)$data) + 1,
                    by = '1 day',
                    length.out = n
                )
            exp(predict(fit, new_dates, interval = 'prediction', level = 0.95)) %>%
                dplyr::as_tibble() %>%
                dplyr::mutate(data = new_dates) %>%
                dplyr::select(data, everything()) %>%
                dplyr::bind_rows(
                    self$get(region) %>%
                        dplyr::select(data, series) %>%
                        dplyr::rename(fit = series) %>%
                        dplyr::mutate(lwr = fit, upr = fit)
                ) %>%
                dplyr::arrange(data) %>%
                dplyr::mutate_if(is.double, as.integer)
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
            tbl_hist <- self$get(region) %>%
                dplyr::mutate(data = datetime_to_timestamp(data))
            hc <-
                highchart() %>%
                hc_chart(type = 'column') %>%
                hc_xAxis(type = 'datetime') %>%
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
                    data = tbl_hist %>%
                        dplyr::select(data, deceduti) %>%
                        list_parse2(),
                    name = 'Deaths'
                ) %>%
                # Terapia intensiva
                hc_add_series(
                    data = tbl_hist %>%
                        dplyr::select(data, terapia_intensiva) %>%
                        list_parse2(),
                    name = 'Intensive Care'
                ) %>%
                # Ricoverati con sintomi
                hc_add_series(
                    data = tbl_hist %>%
                        dplyr::select(data, ricoverati_con_sintomi) %>%
                        list_parse2(),
                    name = 'Hospitalized (no IC)'
                ) %>%
                # Isolamento domiciliare
                hc_add_series(
                    data = tbl_hist %>%
                        dplyr::select(data, isolamento_domiciliare) %>%
                        list_parse2(),
                    name = 'Home Isolation'
                ) %>%
                # Dimessi guariti
                hc_add_series(
                    data = tbl_hist %>%
                        dplyr::select(data, dimessi_guariti) %>%
                        list_parse2(),
                    name = 'Recovered'
                )
            hc
        },

        #' @description
        #' This plots a linechart of the forecast.
        plot_fore = function(region = 'ITA', series = c('totale_casi', 'deceduti'), log = FALSE) {
            labels <- list(
                'totale_casi' = 'Total Cases',
                'deceduti' = 'Deaths'
            )
            hc <-
                highchart() %>%
                hc_xAxis(type = 'datetime')
            if (log) {
                hc %<>%
                    hc_yAxis(
                        type = 'logarithmic'
                    )
            }
            for (field in series) {
                tbl_forecast <-
                    try(
                        self$forecast(region, field) %>%
                            dplyr::mutate(data = datetime_to_timestamp(data)),
                        silent = TRUE
                    )
                if (class(tbl_forecast)[1] == 'try-error')
                    next
                line_color <- private$palette[which(series == field)]
                hc %<>%
                    hc_add_series(
                        data = tbl_forecast %>%
                            dplyr::select(data, fit) %>%
                            list_parse2(),
                        name = labels[[field]],
                        zIndex = 1,
                        marker = list(
                            fillColor = 'white',
                            lineWidth = 1,
                            lineColor = line_color
                        )
                    ) %>%
                    # Terapia intensiva
                    hc_add_series(
                        data = tbl_forecast %>%
                            dplyr::select(data, lwr, upr) %>%
                            list_parse2(),
                        name = 'Range',
                        type = 'arearange',
                        lineWidth = 0,
                        linkedTo = ':previous',
                        color = line_color,
                        fillOpacity = 0.3,
                        zIndex = 0,
                        marker = list(enabled = FALSE)
                    )
            }
            hc
        }

    ),

    # * Private Members ----
    private = list(

        tab = NULL,

        palette = c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#e4d354", "#2b908f", "#f45b5b", "#91e8e1"),
        repo = 'https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv'

    )

)
