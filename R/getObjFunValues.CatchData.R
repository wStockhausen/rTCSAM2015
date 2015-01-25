#'
#'@title Get catch data components in the objective function as a melted dataframe.
#'
#'@description Function to get catch data components in the objective function as a melted dataframe.
#'
#'@param catchData  - a list object reflecting a catch data source's contributions to the objective function
#'@param catch.type - type of catch data ('total.catch','retained.catch','discard.catch')
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
#'@importFrom reshape2 melt
#'
#'@export
#'
getObjFunValues.CatchData<-function(catchData,catch.type="standard"){
        mdfr<-NULL;
        nmctgs<-names(catchData);#names of categories for catchData components
        for (nmctg in nmctgs){
            cat("Processing data category '",nmctg,"'\n",sep='')
            if (tolower(nmctg)=='abundance') {
                mdfrp<-getObjFunValues.AggregateCatch(catchData[[nmctg]],data.type='abundance');
                mdfr<-rbind(mdfr,mdfrp);
            } else                        
            if (tolower(nmctg)=='biomass') {
                mdfrp<-getObjFunValues.AggregateCatch(catchData[[nmctg]],data.type='biomass');
                mdfr<-rbind(mdfr,mdfrp);
            } else
            if (tolower(nmctg)=='n.at.z') {
                mdfrp<-getObjFunValues.NatZ(catchData[[nmctg]]);
                mdfr<-rbind(mdfr,mdfrp);
            } else {
                cat("\tSkipping category\n")
            }
        }#categories
        mdfr<-cbind(list(catch.type=catch.type),mdfr);
    return(mdfr);
}

#mdfr.cd<-getObjFunValues.CatchData(catchData)
