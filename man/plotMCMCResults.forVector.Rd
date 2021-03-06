% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMCMCResults.forVector.R
\name{plotMCMCResults.forVector}
\alias{plotMCMCResults.forVector}
\title{Calculate and plot a posterior density based on a MCMC data vector.}
\usage{
plotMCMCResults.forVector(data, scaleBy = 1, plotEst = TRUE, add = TRUE,
  clr = "cyan", alpha = 0.25, xlim = NULL, ylim = NULL, xlabel = "",
  label = "")
}
\arguments{
\item{data}{- the MCMC data vector from which to estimate the posterior density.}

\item{scaleBy}{- factor to scale data by}

\item{plotEst}{- flag (T/F) to plot the MLE estimate (assumed to be 1st value)}

\item{add}{- flag (T/F) to add to existing plot (creates new plot if FALSE)}

\item{clr}{- color for density plot}

\item{alpha}{- transparency value to apply to clr}

\item{xlim}{- x axis limits (if add=FALSE)}

\item{ylim}{- y axis limits (if add=FALSE)}

\item{xlabel}{- label for x axis (if add=FALSE)}

\item{label}{- label for plot (if add=FALSE)}
}
\description{
Function to calculate and plot a posterior density based on a MCMC data vector.
}
\details{
Uses functions
\itemize{
 \item wtsUtilities::addTransparency(...)
}
}

