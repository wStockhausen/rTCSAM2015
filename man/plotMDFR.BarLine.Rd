% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotMDFR.BarLine.R
\name{plotMDFR.BarLine}
\alias{plotMDFR.BarLine}
\title{Plot melted dataframe using a barchart/line-plot format.}
\usage{
plotMDFR.BarLine(mdfr, x = NULL, value.var = "val", agg.formula = NULL,
  agg.function = sum, ..., colour = NULL, fill = NULL, linetype = NULL,
  shape = NULL, faceting = NULL, xlab = "", ylab = "", units = "",
  lnscale = FALSE, title = "", guideTitleColour = NULL,
  guideTitleFill = NULL, guideTitleLineType = NULL,
  guideTitleShape = NULL, showPlot = TRUE)
}
\arguments{
\item{mdfr}{- melted dataframe}

\item{x}{- column name for x-axis values}

\item{value.var}{- column name for values to aggregate (value.var in cast)/plot on y-axis}

\item{agg.formula}{- aggregation formula (left-hand side of cast formula)}

\item{agg.function}{- aggregation function (fun.aggregate in cast)}

\item{...}{- further arguments passed to aggregating function}

\item{colour}{- column name to which colour aesthetic is mapped}

\item{fill}{- column name to which fill aesthetic is mapped}

\item{linetype}{- column name to which linetype aesthetic is mapped}

\item{shape}{-  column name to which shape aesthetic is mapped}

\item{faceting}{- faceting formula}

\item{xlab}{- x axis label}

\item{ylab}{- y axis label}

\item{units}{- combined with y axis label}

\item{lnscale}{- flag to plot on ln-scale}

\item{title}{- plot title}

\item{guideTitleColour}{- title for colour guide}

\item{guideTitleFill}{- title for fill guide}

\item{guideTitleLineType}{- title for linetype guide}

\item{guideTitleShape}{- title for shape guide}

\item{showPlot}{- flag to show plot immediately}
}
\value{
ggplot2 object
}
\description{
Function to plot a melted dataframe using a barchart/line-plot format.
}

