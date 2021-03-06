% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMDFR.Special.R
\name{getMDFR.GrowthTransitionMatrices}
\alias{getMDFR.GrowthTransitionMatrices}
\title{Get growth transition matrices from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe}
\usage{
getMDFR.GrowthTransitionMatrices(tcsams, rsims, verbose = FALSE)
}
\arguments{
\item{tcsams}{- single TCSAM2015 model report object, or named list of such}

\item{rsims}{- single rsimTCSAM results object, or named list of such}

\item{verbose}{- flag (T/F) to print debug info}
}
\value{
dataframe with columns 'modeltype', 'model', 'pc', 'x', 'z', 'zp', 'val'.
Note that 'z' is pre-molt size, 'zp' is post-molt size.
}
\description{
Function to get growth transition matrices from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
}
\details{
Extracts growth transition matrices.
}

