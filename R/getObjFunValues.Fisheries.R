#'
#'@title Get fishery components in the objective function as a melted dataframe.
#'
#'@description Function to get fishery components in the objective function as a melted dataframe.
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
#'"model", "source.type", "source.name", "catch.type",  "data.type",      
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
getObjFunValues.Fisheries<-function(res,mdl=NULL){
    if (class(res)=='tcsam2015'){
        #res is a tcsam2015 model results object
        dfr<-NULL;
        if (is.null(mdl)) mdl<-res$mc$configName;
        fisheries<-res$model.fits$fisheries;
        nmfshs<-names(fisheries);#names of fisheries
        for (nmfsh in nmfshs){
            cat("fishery is '",nmfsh,"'\n",sep='');
            fishery<-fisheries[[nmfsh]];
            if (!is.null(fishery)){
                nmcts<-names(fishery);#names of catch types
                for (nmct in nmcts){
                    ct<-fishery[[nmct]];#catch type
                    cat("catch type is '",nmct,"'\n",sep='');
                    if (!is.null(ct)){
                        mdfr<-getObjFunValues.CatchData(ct,catch.type=nmct)
                        mdfr<-cbind(list(model=mdl,source.type='fishery',source.name=nmfsh),mdfr);
                        dfr<-rbind(dfr,mdfr);
                    }
                }
            }#non-NULL fishery
        }#fisheries
    } else if (class(res)=='list'){
        #res is a list of tcsam2015 model results objects
        mdls<-names(res);
        dfr<-NULL;
        for (mdl in mdls){
            dfr<-rbind(dfr,getObjFunValues.Fisheries(res[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getObjFunValues.Fisheries(res).\n")
        cat("'res' should be an object of class 'tcsam2015' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(dfr)
}
#mdfr.fshs.1<-getObjFunValues.Fisheries(res)
#mdfr.fshs.2<-getObjFunValues.Fisheries(list(base=res,alt1=res))
