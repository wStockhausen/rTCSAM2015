getObjFunValues.AggregateCatch<-function(aggC){
    
    dfr<-NULL;
    nf<-length(aggC[]);
    for (f in 1:nf){
        rw<-data.frame(
            fit.type=aggC[[f]]$fit.type,
            nll.type=aggC[[f]]$fit$nll.type,
            yr=aggC[[f]]$yr,
            sx=aggC[[f]]$sx,
            ms=aggC[[f]]$ms,
            sc=aggC[[f]]$sc,
            ss=aggC[[f]]$fit$ss,        
            effN=aggC[[f]]$fit$effN,
            wgt=aggC[[f]]$fit$wgt,
            nll=aggC[[f]]$fit$nll,
            objfun=aggC[[f]]$fit$objfun,
            stringsAsFactors=FALSE);
        dfr<-rbind(dfr,rw);
    }
    mdfr<-reshape2::melt(dfr,measure.vars=c('wgt','nll','objfun','ss','effN'))
    return(mdfr);
}

mdfr<-getObjFunValues.AggregateCatch(aggC);