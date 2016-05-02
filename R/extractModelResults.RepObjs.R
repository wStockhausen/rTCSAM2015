#'
#'@title Extract (and optionally write to csv) model results from TCSAM2015 and rsimTCSAM report objects
#'
#'@description Function to extract (and optionally write to csv) model results from TCSAM2015 and rsimTCSAM report objects.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param path - path to object to extract
#'@param cast.formula - casting formula for aggregation, or NULL for no aggregation
#'@param label.value - label for value column in output dataframe/csv
#'@param csv - csv file to write to (can be NULL)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return A dataframe, written to csv if csv was not NULL.
#'
#'@details If csv is NULL, then no file is written, but the created dataframe is still
#'returned. Uses \code{reshape2::dcast} to aggregate using the casting formula.
#'
#'@export
#'
extractModelResults.RepObjs<-function(tcsams=NULL,
                                      rsims=NULL,
                                      path=NULL,
                                      cast.formula=NULL,
                                      label.value='value',
                                      csv=NULL,
                                      verbose=FALSE){
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (inherits(rsims,'rsimTCSAM')){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    if (verbose) cat("Exporting '",path,"'\n",sep='');
    mdfr<-getMDFR(path,tcsams,rsims);
    if (!is.null(cast.formula)) {
        frmla<-as.formula(paste0("modeltype+model+",cast.formula,"~."))
        mdfr<-reshape2::dcast(mdfr,frmla,
                              fun.aggregate=sum,value.var='val');
        idx<-which(names(mdfr)==".");
        names(mdfr)[idx]<-label.value;
    } else {
        idx<-which(names(mdfr)=="val");
        names(mdfr)[idx]<-label.value;
    }
    if (!is.null(csv)) write.csv(mdfr,file=csv,row.names=FALSE);

    if (verbose) cat("Done!\n");
    return(mdfr);
}