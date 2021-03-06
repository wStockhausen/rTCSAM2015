% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runSequence.R
\name{runSequence}
\alias{runSequence}
\title{Function to run a sequence of TCSAM2015 models.}
\usage{
runSequence(os = "osx", path = ".", model = "tcsam2015",
  path2model = "", configFile = "", numRuns = 4, mcmc = FALSE,
  mc.N = 1e+06, mc.save = 1000, mc.scale = 1000, plotResults = FALSE)
}
\arguments{
\item{os}{- 'win' or 'mac' or 'osx'}

\item{path}{- path for model output}

\item{model}{- TCSAM2015 model executable name}

\item{path2model}{- path to model executable}

\item{configFile}{- full (absolute) path to model configuration file}

\item{numRuns}{- number of runs in sequence to make}

\item{mcmc}{- flag (T/F) to run mcmc on "best" model}

\item{mc.N}{- number of mcmc iterations to make}

\item{mc.save}{- number of iterations to skip when saving mcmc calculations}

\item{mc.scale}{- number of iterations to adjust scale for mcmc calculations}

\item{plotResults}{- T/F to plot final results using \code{plotTCSAM2013I}}
}
\value{
- list indicatng index of best run, the folder with the best run, and a list of results 
from the parameter files for each model run.
}
\description{
This functions runs a sequence of TCSAM2015 model.
}
\details{
This function creates a shell script ('./tmp.sh') in the
working directory and uses it to run a version of the TCSAM2015 model. Pin files
are copied from the previous run's par file. The file 'best.txt' identifies the run
with the best objective function value. The "best" sub-folder contains results from
re-running the best run, this time estimating the hessian and obtaining the std file
(if the hessian is invertible).\cr\cr
Uses function \code{wtsUtilities::formatZeros()}.
}

