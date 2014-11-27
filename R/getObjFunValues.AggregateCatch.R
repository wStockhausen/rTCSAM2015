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
    
    dfr<-NULL;
    nf<-names(aggC$fits);
    for (f in nf){
        cat("\t'",f,"'\n",sep='')
        if (!is.null(aggC$fits[[f]])){
            rw<-data.frame(
                data.type=data.type,
                fit.type=aggC$fit.type,
                nll.type=aggC$fits[[f]]$nll.type,
                year=-1,
                sex=f,
                maturity="ALL_MATURITY",
                shell_condition="ALL_SHELL_CONDITION",
                wgt=aggC$fits[[f]]$wgt,
                nll=aggC$fits[[f]]$nll,
                objfun=aggC$fits[[f]]$objfun,
                stringsAsFactors=FALSE);
            dfr<-rbind(dfr,rw);
        }
    }
    mdfr<-reshape2::melt(dfr,measure.vars=c('wgt','nll','objfun'))
    return(mdfr);
}

mdfr.aggC<-getObjFunValues.AggregateCatch(aggC);