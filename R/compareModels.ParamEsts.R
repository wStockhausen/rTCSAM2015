#'
#'@title Function to compare parameter values from different TCSAM2015 models.
#'
#'@description This function extracts and plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard
#'errors from several TCSAM2015 models.
#'
#'@param tcsams - list of TCSAM2015 model results objects (each is a list with elements 'prsObj' and 'stdObj')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param fac - number of std devs to extend uncertainty plots
#'@param nc - number of columns of plots per page
#'@param nr - number of rows of plots per page
#'@param showPlot - flag to show plots immediately
#'@param pdf - file name for printing plots to a pdf file (or NULL to print to screen)
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - list with dfr, vfr, and plots as elements
#'
#'@export
#'
compareModels.ParamEsts<-function(tcsams,dp=0.01,fac=2,
                                  nc=3,nr=4,showPlot=TRUE,
                                  pdf="ModelComparisons.Params.pdf",
                                  verbose=FALSE){
    #extract dataframe with parameter estimates and info
    if (verbose) cat('Extracting params info\n')
    res<-extractModelResults.Params(tcsams,dp=dp,verbose=verbose);
    
    # #extract dataframe with parameter uncertainty info
    # if (verbose) cat("Extracting uncertainty info\n")
    # vfr<-extractModelResults.StdDevs(tcsams,fac=fac,verbose=verbose);
    
    #plot parameters as scalar values
    if (verbose) cat("Plotting parameter results\n")
    plots<-plotModelResults.ScalarParams(dfr=res$prsDFR,
                                         vfr=res$stdDFR,
                                         nc=nc,nr=nr,
                                         showPlot=showPlot,
                                         pdf=pdf,
                                         verbose=verbose);
    
    return(invisible(list(dfr=res$prsDFR,vfr=res$stdDFR,plots=plots)))
}
# resPar<-compareModels.ParamEsts(resLst,dp=0.01,fac=3,
#                                    nc=3,nr=5,showPlot=TRUE)
    
    