#'
#'@title Get aggregate catch components in the objective function as a melted dataframe.
#'
#'@description Function to get aggregate catch components in the objective function as a melted dataframe.
#'
#'@param aggC - a list object reflecting an aggregate catch data source's contributions to the objective function
#'@param data.type - data type (abundance or biomass) of aggregated catch
#'
#'@return a melted dataframe 
#'
#'@details The returned dataframe has columns named 
#'"data.type", "fit.type", "nll.type", "year" (a dummy value = -1), 
#'"sex", "maturity", "shell_condition", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@importFrom reshape2 melt
#'
#'@export
#'
getObjFunValues.AggregateCatch<-function(aggC,
                                         data.type='abundance'){
    mmdfr<-NULL;
    for (n in 1:length(aggC$fits)){
        f<-aggC$fits[[n]];
        if (!is.null(f)){
            dfrp<-data.frame(
                    data.type=data.type,
                    fit.type=aggC$fit.type,
                    nll.type=f$nll$nll.type,
                    year=-1,
                    sex=f$sx,
                    maturity=f$ms,
                    shell_condition=f$sc,
                    wgt=f$nll$wgt,
                    nll=f$nll$nll,
                    objfun=f$nll$objfun,
                    stringsAsFactors=FALSE);
            mdfr<-reshape2::melt(dfrp,measure.vars=c('wgt','nll','objfun'));
            mmdfr<-rbind(mmdfr,mdfr);
        }
    }
    return(mmdfr);
}

#mdfr.aggC<-getObjFunValues.AggregateCatch(aggC);