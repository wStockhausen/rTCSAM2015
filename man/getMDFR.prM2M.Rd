% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Special.R
\name{getMDFR.prM2M}
\alias{getMDFR.prM2M}
\title{Get molt-to-maturity ogives from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe}
\usage{
getMDFR.prM2M(tcsams, rsims, verbose = FALSE)
}
\arguments{
\item{tcsams}{- single TCSAM2015 model report object, or named list of such}

\item{rsims}{- single rsimTCSAM results object, or named list of such}

\item{verbose}{- flag (T/F) to print debug info}
}
\value{
dataframe with columns 'modeltype', 'model', 'pc', 'x', 'z', 'val'.
}
\description{
Function to get molt-to-maturity ogives from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
}
\details{
Extracts molt-to-maturity ogives.
}

