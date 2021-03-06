% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotZScoresForSizeFreqs.R
\name{plotZScoresForSizeFreqs}
\alias{plotZScoresForSizeFreqs}
\title{Plot residuals from size frequency fits}
\usage{
plotZScoresForSizeFreqs(fits, mc, plotPearsons = TRUE, sxs = c(mc$SXs,
  "ALL_SEX"), mss = c(mc$MSs, "ALL_MATURITY"), scs = c(mc$SCs,
  "ALL_SHELL_CONDITION"), label = "")
}
\arguments{
\item{fits}{- list of fits}

\item{mc}{- model configuration list}

\item{plotPearsons}{- flag (T/F) to plot Pearson's residuals (T) or NLLs (F)}

\item{label}{- plot label}
}
\description{
Plot Pearson's residuals or negative log-likelihood components
from size frequency fits.
}

