#' Aides, small and simple functions to help out.
#'
#' Simple utility functions that help with data analysis.
#'
#' @name aides
#' @param x Vector of values.
#' @param y Vector of values.
#' @param digits Number of decimal places for the output.
#'
NULL

#' @rdname aides
#' @export
med <- function(x, digits = 1) {
    format(round(stats::median(x, na.rm = TRUE), digits), nsmall = digits)
}

#' @rdname aides
#' @export
iqr <- function(x, digits = 1) {
    paste0(
        format(round(stats::quantile(x, 0.25, na.rm = TRUE), digits), nsmall = digits),
        ' to ',
        format(round(stats::quantile(x, 0.75, na.rm = TRUE), digits), nsmall = digits)
        )
}

#' @rdname aides
#' @inheritParams stats::cor
#' @export
correlate <- function(x,
                      y,
                      digits = 2,
                      method = 'pearson',
                      use = 'complete.obs') {
    format(round(stats::cor(x, y, use = use,
        method = method), digits), nsmall = digits)
}

#' @rdname aides
#' @export
med_iqr <- function(x, digits = 1) {
    paste0(med(x, digits), ' (', iqr(x, digits), ')')
}

#' @rdname aides
#' @export
average <- function(x, digits = 1) {
    format(round(mean(x, na.rm = TRUE), digits), nsmall = digits)
}

#' @rdname aides
#' @export
stddev <- function(x, digits = 1) {
    format(round(stats::sd(x, na.rm = TRUE), digits), nsmall = digits)
}

#' @rdname aides
#' @export
ave_sd <- function(x, digits = 1) {
    paste0(average(x, digits), ' (', stddev(x, digits), ')')
}

#' @rdname aides
#' @export
tertile <- function(x) {
    cut(x, stats::quantile(x, c(0, .333, .666, 1), na.rm = TRUE),
        include.lowest = TRUE)
}

#' @rdname aides
#' @export
trim_ws <- function (x) {
    gsub("^\\s+|\\s+$", "", x)
}

