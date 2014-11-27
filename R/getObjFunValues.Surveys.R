#'
#'@title Get survey components in the objective function as a melted dataframe.
#'
#'@description Function to get survey components in the objective function as a melted dataframe.
#'
#'@param res - tcsam2015 model results object or list of such
#'@param mdl - name to associate with model results object
#'
#'@return a melted dataframe 
#'
#'@details If res is a list of tcsam2015 model results objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If res is a tcsam2015 model results object and mdl is NULL (the default), then 
#'res$mc$configName is used as the model name.
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
getObjFunValues.Surveys<-function(res,mdl=NULL){
    if (class(res)=='tcsam2015'){
        #res is a tcsam2015 model results object
        dfr<-NULL;
        if (is.null(mdl)) mdl<-res$mc$configName;
        surveys<-res$model.fits$surveys;
        nmsrvs<-names(surveys);
        for (nmsrv in nmsrvs){
            survey<-surveys[[nmsrv]];
            if (!is.null(survey)){
                mdfr<-getObjFunValues.CatchData(survey,catch.type='total')
                mdfr<-cbind(list(model=mdl,source.type='survey',source.name=nmsrv),mdfr);
                dfr<-rbind(dfr,mdfr);
            }#non-NULL survey
        }#surveys
    } else if (class(res)=='list'){
        #res is a list of tcsam2015 model results objects
        mdls<-names(res);
        dfr<-NULL;
        for (mdl in mdls){
            dfr<-rbind(dfr,getObjFunValues.Surveys(res[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getObjFunValues.Surveys(res).\n")
        cat("'res' should be an object of class 'tcsam2015' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(dfr)
}
mdfr.srvs.1<-getObjFunValues.Surveys(res)
mdfrsrvs.2<-getObjFunValues.Surveys(list(base=res,alt1=res))
