% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTCSAM2015I.R
\name{plotTCSAM2015I}
\alias{plotTCSAM2015I}
\title{Plot TCSAM2015 model output}
\usage{
plotTCSAM2015I(repObj = NULL, prsObj = NULL, stdObj = NULL,
  objList = NULL, ggtheme = theme_grey(), showPlot = TRUE, pdf = NULL,
  width = 14, height = 8, verbose = FALSE)
}
\arguments{
\item{repObj}{- tcsam2015.rep object based on sourcing a TCSAM2015 model report file. can be NULL.}

\item{prsObj}{- tcsam2015.prs object based on reading a TCSAM2015 active parameters csv file. can be NULL.}

\item{stdObj}{- tcsam2015.std object based on reading a TCSAM2015 std file. can be NULL.}

\item{objList}{- list with optional elements repObj, prsObj, stdObj (an optional way to provide the Obj's)}

\item{ggtheme}{- a ggplot2 theme to use with ggplot2 plots}

\item{showPlot}{- flag to show plots immediately}

\item{pdf}{- filename for pdf output (optional)}

\item{width}{- pdf page width (in inches)}

\item{height}{- pdf page width (in inches)}

\item{verbose}{- flag (T/F) to print diagnostic info}
}
\value{
multi-level list of ggplot2 objects
}
\description{
Function to plot data and results from a TCSAM2015 model run.
}
\details{
none
}

