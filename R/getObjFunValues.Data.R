#'
#'@title Get data components in the objective function as a melted dataframe.
#'
#'@description Function to get data (fisheries, surveys) components in the objective function as a melted dataframe.
#'
#'@param repObjs - tcsam2015 model results object or list of such
#'@param mdl - name to associate with model results object
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details If repObjs is a list of tcsam2015 model results objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If repObjs is a tcsam2015 model results object and mdl is NULL (the default), then 
#'repObjs$mc$configName is used as the model name.
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
getObjFunValues.Data<-function(repObjs,mdl=NULL,verbose=FALSE){
    dfr<-getObjFunValues.Fleets(repObjs,mdl,'survey',verbose);
    dfr<-rbind(dfr,getObjFunValues.Fleets(repObjs,mdl,'fishery',verbose));
    return(dfr)
}
#mdfr.data.1<-getObjFunValues.Data(repObjs)
#mdfr.data.2<-getObjFunValues.Data(list(base=repObjs,alt1=repObjs))
