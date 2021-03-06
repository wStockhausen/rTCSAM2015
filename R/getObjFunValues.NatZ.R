#'
#'@title Get size comps components in the objective function as a melted dataframe.
#'
#'@description Function to get size comps components in the objective function as a melted dataframe.
#'
#'@param nAtZ - a list object reflecting a size comps' contributions to the objective function
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details The returned dataframe has columns named 
#'"data.type", "fit.type", "nll.type", "year", 
#'"sex", "maturity", "shell_condition", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), objective function value ('objfun'), input
#'sample size ('ss') or effective sample size ('effN').
#'
#'@export
#'
getObjFunValues.NatZ<-function(nAtZ,
                               verbose=FALSE){    
    dfr<-NULL;
    nf<-length(nAtZ[]);
    for (f in 1:nf){
        if (!is.null(nAtZ[[f]])){
            rw<-data.frame(
                data.type='n.at.z',
                fit.type=nAtZ[[f]]$fit.type,
                nll.type=nAtZ[[f]]$fit$nll.type,
                year=nAtZ[[f]]$y,
                sex=nAtZ[[f]]$x,
                maturity=nAtZ[[f]]$m,
                shell_condition=nAtZ[[f]]$s,
                ss=nAtZ[[f]]$fit$ss,        
                effN=nAtZ[[f]]$fit$effN,
                wgt=nAtZ[[f]]$fit$wgt,
                nll=nAtZ[[f]]$fit$nll,
                objfun=nAtZ[[f]]$fit$objfun,
                stringsAsFactors=FALSE);
            dfr<-rbind(dfr,rw);
        }
    }
    mdfr<-reshape2::melt(dfr,measure.vars=c('wgt','nll','objfun','ss','effN'))
    return(mdfr);
}

#mdfr.nAtZ<-getObjFunValues.NatZ(nAtZ);