#'
#'@title Get a dataframe describing the input data time frames
#'
#'@description A function to get a dataframe that describes the input data time frames.
#'
#'@param tcsam - tcsam list object 
#'@param verbose - flag (T/F) to print diagnostic info
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
getDFR.InputDataTimeframes<-function(tcsam,verbose=FALSE){
    if (class(tcsam)!='tcsam2015'){
        cat("Error in getDFR.InputDataTimeframes().\n");
        cat("Input object hass class",class(tcsam),"not class 'tcsam'\n");
        exit(-1);
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
