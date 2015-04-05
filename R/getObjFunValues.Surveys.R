#'
#'@title Get survey components in the objective function as a melted dataframe.
#'
#'@description Function to get survey components in the objective function as a melted dataframe.
#'
#'@param repObj - tcsam2015 model report object or list of such
#'@param model - name to associate with model report object
#'
#'@return a melted dataframe 
#'
#'@details If repObj is a list of tcsam2015 model report objects, then the function
#'is called recursively for each object, with the associated list component name used as the
#'model. If repObj is a tcsam2015 model report object and model is NULL (the default), then 
#'repObj$mc$configName is used as the model name.
#'
#'The returned dataframe has columns named 
#'"model", "source.type", "source.name", "catch.type", "data.type",       
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
getObjFunValues.Surveys<-function(repObj,model=NULL){
    if (class(repObj)=='tcsam2015'){
        #repObj is a tcsam2015 model results object
        dfr<-NULL;
        if (is.null(model)) model<-repObj$mc$configName;
        surveys<-repObj$model.fits$surveys;
        nmsrvs<-names(surveys);
        for (nmsrv in nmsrvs){
            cat("survey is '",nmsrv,"'\n",sep='');
            survey<-surveys[[nmsrv]];
            if (!is.null(survey)){
                mdfr<-getObjFunValues.CatchData(survey,catch.type='total.catch')
                mdfr<-cbind(list(model=model,source.type='survey',source.name=nmsrv),mdfr);
                dfr<-rbind(dfr,mdfr);
            }#non-NULL survey
        }#surveys
    } else if (class(repObj)=='list'){
        #repObj is a list of tcsam2015 model report objects
        models<-names(repObj);
        dfr<-NULL;
        for (model in models){
            dfr<-rbind(dfr,getObjFunValues.Surveys(repObj[[model]],model=model));
        }
    } else {
        cat("Error in getObjFunValues.Surveys(repObj).\n")
        cat("'repObj' should be an object of class 'tcsam2015' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(dfr)
}
#mdfr.srvs.1<-getObjFunValues.Surveys(repObj)
#mdfrsrvs.2<-getObjFunValues.Surveys(list(base=repObj,alt1=repObj))
