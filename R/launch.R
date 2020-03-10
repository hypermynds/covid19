#' Run the graphical interface in a web browser
#' @export
launch <- function() {
    rmarkdown::run(
        system.file('dashboard/covid19.Rmd', package = 'covid19')
    )
}
