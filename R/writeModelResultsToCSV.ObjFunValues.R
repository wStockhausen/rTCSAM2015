#'
#'@title Write all objective function values to csvs
#'
#'@description Function to write all objective function values to csvs.
#'
#'@param repObjs - model report object or list of report objects
#'@param csv - base filename for csvs to export dataframes to
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return list w two dataframes with objective function components, 
#'one for penalties and priors (...PP.csv) and one for data (...Data.csv).
#'
#'@export
#'
writeModelResultsToCSV.ObjFunValues<-function(repObjs,
                                              csv="ModelComparisons.ObjFunValues",
                                              verbose=FALSE){
    if (verbose) cat("plotting objective function values for penalties.\n")
    mdfr1<-getObjFunValues.Penalties(repObjs,verbose=verbose);

    if (verbose) cat("plotting objective function values for priors.\n")
    mdfr2<-getObjFunValues.Priors(repObjs,verbose=verbose);
    
    mdfr<-rbind(mdfr1,mdfr2);
    dfr<-reshape2::dcast(mdfr,"type+category+name+level~variable+model",value.var="value")
    write.csv(dfr,file=paste0(csv,".PP.csv"),row.names=FALSE)

    if (verbose) cat("plotting objective function values for data.\n")
    mdfr3<-getObjFunValues.Data(repObjs,verbose=verbose);
    dfr3a<-reshape2::dcast(mdfr3,
                           "source.type+source.name+catch.type+data.type+fit.type+nll.type+sex+maturity+shell_condition~variable+model",
                           fun.aggregate=sum,
                           value.var="value")
    write.csv(dfr,file=paste0(csv,".Data.csv"),row.names=FALSE)
    
    return(list(pp=dfr,data=dfr3a));
}

