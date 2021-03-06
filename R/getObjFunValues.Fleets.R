#'
#'@title Get fleet data components in the objective function as a melted dataframe
#'
#'@description Function to get fleet data components in the objective function as a melted dataframe.
#'
#'@param repObjs - tcsam2015 model report object or list of such
#'@param mdl - name to associate with model results object
#'@param type - "fishery" or "survey"
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details If repObjs is a list of tcsam2015 model report objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If repObjs is a tcsam2015 model report object and mdl is NULL (the default), then 
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
getObjFunValues.Fleets<-function(repObjs,mdl=NULL,type='fishery',verbose=FALSE){
    if (inherits(repObjs,'tcsam2015.rep')){
        #repObjs is a tcsam2015 model results object
        dfr<-NULL;
        if (is.null(mdl)) mdl<-repObjs$mc$configName;
        if (type=='fishery'){
            fleets<-repObjs$model.fits$fisheries;
        } else if (type=='survey'){
            fleets<-repObjs$model.fits$surveys;
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
    } else if (class(repObjs)=='list'){
        #repObjs is a list of tcsam2015 model report objects
        mdls<-names(repObjs);
        dfr<-NULL;
        for (mdl in mdls){
            dfr<-rbind(dfr,getObjFunValues.Fleets(repObjs[[mdl]],mdl=mdl,type=type,verbose=verbose));
        }
    } else {
        cat("Error in getObjFunValues.Fleets(repObjs).\n")
        cat("'repObjs' should be an object of class 'tcsam2015.rep' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(dfr)
}
#mdfr.fshs.1<-getObjFunValues.Fleets(repObjs)
#mdfr.fshs.2<-getObjFunValues.Fleets(list(base=repObjs,alt1=repObjs))
