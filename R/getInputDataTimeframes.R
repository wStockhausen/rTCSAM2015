#'
#'@title Get a dataframe describing the input data time frames
#'
#'@description A function to get a dataframe that describes the input data time frames.
#'
#'@param tcsam - tcsam2015.rep list object 
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return A dataframe.
#'
#'@details returns a dataframe with columns
#'\itemize{
#'  \item year
#'  \item model
#'  \item fleet type
#'  \item fleet
#'  \item catch type
#'  \item data type
#'}
#'
#'@export
#'
getInputDataTimeframes<-function(tcsam,verbose=FALSE){
    if (!inherits(tcsam,'tcsam2015.rep')){
        cat("Error in getInputDataTimeframes().\n");
        cat("Input object hass class",class(tcsam),"not class 'tcsam2015.rep'\n");
        return(NULL);
    }
    srvs <- tcsam$mc$dims$v$nms;#survey names
    fshs <- tcsam$mc$dims$f$nms;#fishery names

    mdfr<-NULL;
    
    #surveys
    base<-'data/surveys';
    catchtypes<-c("index.catch");
    datatypes<-c("abundance","biomass","nAtZ");
    for (flt in srvs){
        for (ct in catchtypes){
            for (dt in datatypes){
                pth<-paste(base,flt,ct,dt,'y',sep='/');
                if(verbose) cat(pth,'\n');
                dfr<-getMDFR(pth,tcsam=tcsam);
                if (!is.null(dfr)){
                    dfr<-dfr[,2:3];
                    names(dfr)<-c("year","model");
                    dfr$flttype<-'survey';
                    dfr$fltname<-flt;
                    dfr$catchtype<-ct;
                    dfr$datatype <-dt;
                    mdfr<-rbind(mdfr,dfr);
                }
            }##datatypes
        }##catchtypes
    }##fleets
    
    #fisheries
    base<-'data/fisheries';
    catchtypes<-c("retained.catch",
                  "discard.catch",
                  "total.catch",
                  "effort");
    datatypes<-c("abundance","biomass","nAtZ");
    for (flt in fshs){
        for (ct in catchtypes){
            for (dt in datatypes){
                pth<-paste(base,flt,ct,dt,'y',sep='/');
                if(verbose) cat(pth,'\n');
                dfr<-getMDFR(pth,tcsam=tcsam);
                if (!is.null(dfr)){
                    dfr<-dfr[,2:3];
                    names(dfr)<-c("year","model");
                    dfr$flttype<-'fishery';
                    dfr$fltname<-flt;
                    dfr$catchtype<-ct;
                    dfr$datatype <-dt;
                    mdfr<-rbind(mdfr,dfr);
                }
            }##datatypes
        }##catchtypes
    }##fleets
    if (!is.null(mdfr)){
        names(mdfr)<-c("year",
                       "model",
                       "fleet type",
                       "fleet",
                       "catch type",
                       "data type");
    }
    return(mdfr);
}
