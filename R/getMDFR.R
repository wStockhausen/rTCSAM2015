#'
#'@title Get model arrays as a melted dataframe from TCSAM2015 and rsimTCSAM models.
#'
#'@description Function to get model objects as a melted dataframe from TCSAM2015 and rsimTCSAM models.
#'
#'@param path - path in models to requested array (using '/' as separator for list levels)
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print diagnostics
#'
#'@return Melted dataframe (ala package reshape2) of the requested array, with additional columns 'model' and 'modeltype'.
#'
#'@export
#'
getMDFR<-function(path,tcsam=NULL,rsim=NULL,verbose=FALSE){
    mdfr<-NULL;
    if (!is.null(tcsam)){
        if (class(tcsam)=='tcsam2015'){
            #tcsam is a tcsam2015 model object
            obj<-getObj(path,tcsam,verbose=verbose);
            if (!is.null(obj)){
                mdfr<-reshape2::melt(obj,value.name='val',as.is=TRUE);
                mdfr$model<-'tcsam';
                mdfr$modeltype<-'tcsam';
            }
        } else if (class(tcsam)=='list'){
            #tcsam is a list of tcsam2015 model objects
            nl<-length(tcsam);
            nms<-names(tcsam);
            for (l in 1:nl){
                tcsam1<-tcsam[[l]];
                mdfrp<-getMDFR(path,tcsam=tcsam1,rsim=NULL,verbose=verbose);
                if (!is.null(mdfrp)){
                    if (!is.null(nms[l])) mdfrp$model<-nms[l];
                    mdfrp$modeltype<-'tcsam';
                    mdfr<-rbind(mdfr,mdfrp);
                }
            }
        } else {
            cat("Error in getMDFR(path,tcsam,rsim).\n")
            cat("'tcsam' should be NULL, an object of class 'tcsam2015', or a list of such.\n")
            cat("Returning NULL.\n")
            return(NULL);
        }
    }
    
    if (!is.null(rsim)){
        if (class(rsim)=='rsimTCSAM'){
            #rsim is a rsimTCSAM model object
            obj<-getObj(path,rsim,verbose=verbose);
            if (!is.null(obj)){
                mdfrp<-reshape2::melt(obj,value.name='val',as.is=TRUE);
                mdfrp$model<-'rsim';
                mdfrp$modeltype<-'rsim';
                mdfr<-rbind(mdfr,mdfrp)
            }
        } else if (class(rsim)=='list'){
            #rsim is a list of rsimTCSAM model objects
            nl<-length(rsim);
            nms<-names(rsim);
            for (l in 1:nl){
                rsim1<-rsim[[l]];
                mdfrp<-getMDFR(path,rsim=rsim1,tcsam=NULL,verbose=verbose);
                if (!is.null(mdfrp)){
                    if (!is.null(nms[l])) mdfrp$model<-nms[l];
                    mdfrp$modeltype<-'rsim';
                    mdfr<-rbind(mdfr,mdfrp);
                }
            }
        } else {
            cat("Error in getMDFR(path,tcsam,rsim).\n")
            cat("'rsim' should be NULL, an object of class 'rsimTCSAM', or a list of such.\n")
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
