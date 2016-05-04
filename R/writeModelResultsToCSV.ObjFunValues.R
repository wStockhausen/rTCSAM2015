#'
#'@title Plot all objective function values
#'
#'@description Function to plot all objective function values.
#'
#'@param repObjs - model report object or list of report objects
#'@param variable - name of variable to plot
#'@param csv - csf file name to export dataframe to
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe with objective function components for penalties, priors, and data.
#'
#'@export
#'
writeModelResultsToCSV.ObjFunValues<-function(repObjs,
                                              variable='objfun',
                                              csv="ModelComparisons.ObjFunValues.csv",
                                              verbose=FALSE){
    if (verbose) cat("plotting objective function values for penalties.\n")
    mdfr<-getObjFunValues.Penalties(repObjs,verbose=verbose);

    if (verbose) cat("plotting objective function values for priors.\n")
    mdfr<-getObjFunValues.Priors(repObjs,verbose=verbose);

    if (verbose) cat("plotting objective function values for data.\n")
    mdfr<-getObjFunValues.Data(repObjs,verbose=verbose);
    
    return(plots);
}

#plots<-compareModelResults.ObjFunValues(repObjs); print(plots);