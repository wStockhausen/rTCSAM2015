#'
#'@title Get daat components in the objective function as a melted dataframe.
#'
#'@description Function to get data (fisheries, surveys) components in the objective function as a melted dataframe.
#'
#'@param res - tcsam2015 model results object or list of such
#'@param mdl - name to associate with model results object
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details If res is a list of tcsam2015 model results objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If res is a tcsam2015 model results object and mdl is NULL (the default), then 
#'res$mc$configName is used as the model name.
#'
#'The returned dataframe has columns named 
#'"model", "source.type", "source.name", "catch.type",  "data.type",      
#'"fit.type", "nll.type", "year", "sex",
#'"maturity", "shell_condition", "variable", "value"
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getObjFunValues.Data<-function(res,mdl=NULL,verbose=FALSE){
    dfr<-getObjFunValues.Fleets(res,mdl,'survey',verbose);
    dfr<-rbind(dfr,getObjFunValues.Fleets(res,mdl,'fishery',verbose));
    return(dfr)
}
#mdfr.data.1<-getObjFunValues.Data(res)
#mdfr.data.2<-getObjFunValues.Data(list(base=res,alt1=res))
