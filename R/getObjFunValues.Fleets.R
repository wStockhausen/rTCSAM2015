#'
#'@title Get fleet data components in the objective function as a melted dataframe
#'
#'@description Function to get fleet data components in the objective function as a melted dataframe.
#'
#'@param res - tcsam2015 model report object or list of such
#'@param mdl - name to associate with model results object
#'@param type - "fishery" or "survey"
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details If res is a list of tcsam2015 model report objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If res is a tcsam2015 model report object and mdl is NULL (the default), then 
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
getObjFunValues.Fleets<-function(res,mdl=NULL,type='fishery',verbose=FALSE){
    if (class(res)=='tcsam2015.rep'){
        #res is a tcsam2015 model results object
        dfr<-NULL;
        if (is.null(mdl)) mdl<-res$mc$configName;
        if (type=='fishery'){
            fleets<-res$model.fits$fisheries;
        } else if (type=='survey'){
            fleets<-res$model.fits$surveys;
        } else {
            
        }
        nmFlts<-names(fleets);#names of fleets
        for (nmFlt in nmFlts){
            if (verbose) cat("fleet is '",nmFlt,"'\n",sep='');
            fleet<-fleets[[nmFlt]];
            if (!is.null(fleet)){
                nmcts<-names(fleet);#names of catch types
                for (nmct in nmcts){
                    ct<-fleet[[nmct]];#catch type
                    if (verbose) cat("catch type is '",nmct,"'\n",sep='');
                    if (!is.null(ct)){
                        mdfr<-getObjFunValues.CatchData(ct,catch.type=nmct)
                        mdfr<-cbind(list(model=mdl,source.type=type,source.name=nmFlt),mdfr);
                        dfr<-rbind(dfr,mdfr);
                    }
                }
            }#non-NULL fleet
        }#fleets
    } else if (class(res)=='list'){
        #res is a list of tcsam2015 model report objects
        mdls<-names(res);
        dfr<-NULL;
        for (mdl in mdls){
            dfr<-rbind(dfr,getObjFunValues.Fleets(res[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getObjFunValues.Fleets(res).\n")
        cat("'res' should be an object of class 'tcsam2015.rep' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(dfr)
}
#mdfr.fshs.1<-getObjFunValues.Fleets(res)
#mdfr.fshs.2<-getObjFunValues.Fleets(list(base=res,alt1=res))
