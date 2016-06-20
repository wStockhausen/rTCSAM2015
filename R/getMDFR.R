#'
#'@title Get model arrays as a melted dataframe from TCSAM2015 and rsimTCSAM models
#'
#'@description Function to get model objects as a melted dataframe from TCSAM2015 and rsimTCSAM models.
#'
#'@param path - path in models to requested array (using '/' as separator for list levels)
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print diagnostics
#'
#'@return Melted dataframe (ala package reshape2).
#'
#'@details Returned dataframe is a melted (ala reshape2) version of the requested array, 
#'with additional columns 'model' and 'modeltype' appended at the "right". The array value
#'is in column 'val'. Uses \code{reshape2::melt(...)}.
#'
#'@export
#'
getMDFR<-function(path,tcsams=NULL,rsims=NULL,verbose=FALSE){
    mdfr<-NULL;
    if (!is.null(tcsams)){
        if (inherits(tcsams,'tcsam2015.rep')){
            #tcsams is a tcsam2015 model report object
            obj<-getObj(path,tcsams,verbose=verbose);
            if (!is.null(obj)){
                mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
                mdfr$model<-'tcsam';
                mdfr$modeltype<-'TCSAM2015';
            }
        } else if (class(tcsams)=='list'){
            #tcsams is a list of tcsam2015 model report objects
            nl<-length(tcsams);
            nms<-names(tcsams);
            for (l in 1:nl){
                tcsam1<-tcsams[[l]];
                mdfrp<-getMDFR(path,tcsams=tcsam1,rsims=NULL,verbose=verbose);
                if (!is.null(mdfrp)){
                    if (!is.null(nms[l])) mdfrp$model<-nms[l];
                    mdfr<-rbind(mdfr,mdfrp);
                }
            }
        } else {
            cat("Error in getMDFR(path,tcsams,rsims).\n")
            cat("'tcsams' should be NULL, an object of class 'tcsam2015.rep', or a list of such.\n")
            cat("Returning NULL.\n")
            return(NULL);
        }
    }
    
    if (!is.null(rsims)){
        if (inherits(rsims,'rsimTCSAM')){
            #rsims is a rsimTCSAM model object
            obj<-getObj(path,rsims,verbose=verbose);
            if (!is.null(obj)){
                mdfrp<-reshape2::melt(obj,value.name='val',as.is=TRUE);
                mdfrp$model<-'rsim';
                mdfrp$modeltype<-'rsim';
                mdfr<-rbind(mdfr,mdfrp)
            }
        } else if (class(rsims)=='list'){
            #rsims is a list of rsimTCSAM model objects
            nl<-length(rsims);
            nms<-names(rsims);
            for (l in 1:nl){
                rsim1<-rsims[[l]];
                mdfrp<-getMDFR(path,rsims=rsim1,tcsams=NULL,verbose=verbose);
                if (!is.null(mdfrp)){
                    if (!is.null(nms[l])) mdfrp$model<-nms[l];
                    mdfrp$modeltype<-'rsim';
                    mdfr<-rbind(mdfr,mdfrp);
                }
            }
        } else {
            cat("Error in getMDFR(path,tcsams,rsims).\n")
            cat("'rsims' should be NULL, an object of class 'rsimTCSAM', or a list of such.\n")
            cat("Returning NULL.\n")
            return(NULL);
        }
    }
    
    if (!is.null(mdfr)){
        cns<-colnames(mdfr);
        chks<-c('y','z','zp');
        for (chk in chks){
            idx<-which(cns==chk);
            if (length(idx)>0) mdfr[,chk]<-as.numeric(mdfr[,chk]);
        }
    }
    return(mdfr)
}
