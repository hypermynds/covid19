#' Simple Cap Function
#'
#' @param s is the string to parse.
#' @param strict is a flag that allows to parse an entire word.
#' @export
simple_cap <- function(s, strict = FALSE) {
    if (length(s) > 1) {
        sapply(s, simple_cap, strict = strict, USE.NAMES = FALSE)
    } else {
        cap <- function(s)
            paste(
                toupper(substring(s, 1, 1)),
                {s <- substring(s, 2); if (strict) tolower(s) else s},
                sep = "", collapse = " "
            )
        put_apo <- function(old, new) {
            idx <- lapply(strsplit(old, ''), function(x) which(x == '\''))[[1]]
            splitword <- strsplit(new, '')[[1]]
            splitword[idx] <- '\''
            paste(splitword, collapse = '')
        }
        s_mod <- sapply(strsplit(s, split = ' |\''), cap, USE.NAMES = !is.null(names(s)))
        put_apo(s, s_mod)
    }
}
