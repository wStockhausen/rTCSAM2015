#'
#'@title Get catch data components in the objective function as a melted dataframe.
#'
#'@description Function to get catch data components in the objective function as a melted dataframe.
#'
#'@param catchData  - a list object reflecting a catch data source's contributions to the objective function
#'@param catch.type - type of catch data ('total.catch','retained.catch','discard.catch')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details The returned dataframe has columns named 
#'"catch.type", "data.type",      
#'"fit.type", "nll.type", "year", "sex",
#'"maturity", "shell_condition", "variable", "value"
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getObjFunValues.CatchData<-function(catchData,
                                    catch.type="total.catch",
                                    verbose=FALSE){
        mdfr<-NULL;
        nmctgs<-names(catchData);#names of categories for catchData components
        for (nmctg in nmctgs){
            if (verbose) cat("Processing data category '",nmctg,"'\n",sep='')
            if (tolower(nmctg)=='abundance') {
                mdfrp<-getObjFunValues.AggregateCatch(catchData[[nmctg]],data.type='abundance',verbose=verbose);
                mdfr<-rbind(mdfr,mdfrp);
            } else                        
            if (tolower(nmctg)=='biomass') {
                mdfrp<-getObjFunValues.AggregateCatch(catchData[[nmctg]],data.type='biomass',verbose=verbose);
                mdfr<-rbind(mdfr,mdfrp);
            } else
            if (tolower(nmctg)=='n.at.z') {
                mdfrp<-getObjFunValues.NatZ(catchData[[nmctg]],verbose=verbose);
                mdfr<-rbind(mdfr,mdfrp);
            } else {
                if (verbose) cat("\tSkipping category\n")
            }
        }#categories
        mdfr<-cbind(list(catch.type=catch.type),mdfr);
    return(mdfr);
}

#mdfr.cd<-getObjFunValues.CatchData(catchData)
