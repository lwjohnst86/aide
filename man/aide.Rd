% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aide.R
\name{aide}
\alias{aide}
\alias{med}
\alias{iqr}
\alias{correlate}
\alias{med_iqr}
\alias{average}
\alias{stddev}
\alias{ave_sd}
\alias{tertile}
\alias{min_max}
\alias{format_round}
\alias{format_pval}
\title{aide, small and simple functions to help out.}
\usage{
med(x, digits = 1)

iqr(x, digits = 1)

correlate(x, y, digits = 2, method = "pearson", use = "complete.obs")

med_iqr(x, digits = 1)

average(x, digits = 1)

stddev(x, digits = 1)

ave_sd(x, digits = 1)

tertile(x)

min_max(x, digits = 1)

format_round(x, digits = 1)

format_pval(x, digits = 3)
}
\arguments{
\item{x}{Vector of values.}

\item{digits}{Number of decimal places for the output.}

\item{y}{Vector of values.}

\item{method}{a character string indicating which correlation
    coefficient (or covariance) is to be computed.  One of
    \code{"pearson"} (default), \code{"kendall"}, or \code{"spearman"}:
    can be abbreviated.}

\item{use}{an optional character string giving a
    method for computing covariances in the presence
    of missing values.  This must be (an abbreviation of) one of the strings
    \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"},
    \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.}
}
\description{
Simple utility functions that help with data analysis.
}
