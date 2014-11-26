getObjFunValues.NatZ<-function(nAtZ){
    
    dfr<-NULL;
    nf<-length(nAtZ[]);
    for (f in 1:nf){
        rw<-data.frame(
            fit.type=nAtZ[[f]]$fit.type,
            nll.type=nAtZ[[f]]$fit$nll.type,
            yr=nAtZ[[f]]$yr,
            sx=nAtZ[[f]]$sx,
            ms=nAtZ[[f]]$ms,
            sc=nAtZ[[f]]$sc,
            ss=nAtZ[[f]]$fit$ss,        
            effN=nAtZ[[f]]$fit$effN,
            wgt=nAtZ[[f]]$fit$wgt,
            nll=nAtZ[[f]]$fit$nll,
            objfun=nAtZ[[f]]$fit$objfun,
            stringsAsFactors=FALSE);
        dfr<-rbind(dfr,rw);
    }
    mdfr<-reshape2::melt(dfr,measure.vars=c('wgt','nll','objfun','ss','effN'))
    return(mdfr);
}

mdfr<-getObjFunValues.NatZ(nAtZ);