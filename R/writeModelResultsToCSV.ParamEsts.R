#'
#'@title Function to write parameter values and standard deviations from different TCSAM2015 models to a csv file.
#'
#'@description This function extracts parameters values, together with their limits
#'(if any) and their associated standard deviations from several TCSAM2015 models as a dataframe
#'and writes it to a csv file.
#'
#'@param tcsams - list of TCSAM2015 model results objects (each is a list with elements 'prsObj' and 'stdObj')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - dataframe returned by \code{extractModelResults.ParamsPlusStdDevs}
#'
#'@export
#'
writeModelResultsToCSV.ParamEsts<-function(tcsams,
                                           dp=0.01,
                                           csv="ModelComparisons.Params.csv",
                                           verbose=FALSE){
    #extract dataframe with parameter estimates and info
    if (verbose) cat('Extracting params info\n')
    dfr<-extractModelResults.ParamsPlusStdDevs(tcsams,dp=dp,verbose=verbose);
    
    write.csv(dfr,file=csv,row.names=FALSE);

    return(dfr);
}
# resPar<-compareModels.ParamEsts(resLst,dp=0.01,fac=3,
#                                    nc=3,nr=5,showPlot=TRUE)
    
    