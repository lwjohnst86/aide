#' aide, small and simple functions to help out.
#'
#' Simple utility functions that help with data analysis.
#'
#' @name aide
#' @param x Vector of values.
#' @param y Vector of values.
#' @param digits Number of decimal places for the output.
#'
NULL

#' @rdname aide
#' @export
med <- function(x, digits = 1) {
    format_round(stats::median(x, na.rm = TRUE), digits)
}

#' @rdname aide
#' @export
iqr <- function(x, digits = 1) {
    lower <- format_round(stats::quantile(x, 0.25, na.rm = TRUE), digits)
    upper <- format_round(stats::quantile(x, 0.75, na.rm = TRUE), digits)
    paste0(lower, ' to ', upper)
}

#' @rdname aide
#' @inheritParams stats::cor
#' @export
correlate <- function(x,
                      y,
                      digits = 2,
                      method = 'pearson',
                      use = 'complete.obs') {
    format_round(stats::cor(x, y, use = use, method = method), digits)
}

#' @rdname aide
#' @export
med_iqr <- function(x, digits = 1) {
    paste0(med(x, digits), ' (', iqr(x, digits), ')')
}

#' @rdname aide
#' @export
average <- function(x, digits = 1) {
    format_round(mean(x, na.rm = TRUE), digits)
}

#' @rdname aide
#' @export
stddev <- function(x, digits = 1) {
    format_round(stats::sd(x, na.rm = TRUE), digits)
}

#' @rdname aide
#' @export
ave_sd <- function(x, digits = 1) {
    paste0(average(x, digits), ' (', stddev(x, digits), ')')
}

#' @rdname aide
#' @export
tertile <- function(x) {
    cut(x, stats::quantile(x, c(0, .333, .666, 1), na.rm = TRUE),
        include.lowest = TRUE)
}

#' @rdname aide
#' @export
trim_ws <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
}

#' @rdname aide
#' @export
min_max <- function(x, digits = 1) {
    minimum <- format_round(min(x, na.rm = TRUE), digits)
    maximum <- format_round(max(x, na.rm = TRUE), digits)
    paste0(minimum, " to ", maximum)

}

format_round <- function(x, digits = 1) {
    format(round(x, digits = digits), nsmall = digits)
}