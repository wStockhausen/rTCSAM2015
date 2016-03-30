#'
#'@title Get model penalties in the objective function as a melted dataframe
#'
#'@description Function to get model penalties as a melted dataframe.
#'
#'@param repObj - tcsam2015.rep model results object or list of such
#'@param mdl - name to associate with model results object
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return a melted dataframe 
#'
#'@details If repObj is a list of tcsam2015 model results objects, then the function
#'is called recursively for each object, with the associated list component name used as 
#'mdl. If repObj is a tcsam2015 model results object and mdl is NULL (the default), then 
#'repObj$mc$configName is used as the model name.
#'
#'The returned dataframe has columns named 
#'"model", "type", "category", "name", "level", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getObjFunValues.Penalties<-function(repObj,
                                    mdl=NULL,
                                    verbose=FALSE){
    if (class(repObj)=='tcsam2015.rep'){
        #repObj is a tcsam2015 model results object
        if (is.null(mdl)) mdl<-repObj$mc$configName;
        penalties<-repObj$model.fits$penalties;
        nmctgs<-names(penalties);#names of model categories for penalties
        dfr<-NULL;
        for (nmctg in nmctgs){
            if(verbose) cat("Processing penalties for category",nmctg,'\n')
            ctg<-penalties[[nmctg]];#model category object
            nmps<-names(ctg);   #names of penalties in category
            for (nmp in nmps){
                if(verbose) cat("Processing penalty for",nmp,'\n')
                pen<-ctg[[nmp]];
                if (!is.null(pen)){
                    nmlevs<-names(pen);
                    for (nmlev in nmlevs){
                        if(verbose) cat("\tProcessing level",nmlev,'\n')
                        lev<-pen[[nmlev]]
                        if (!is.null(lev)){
                            rw<-data.frame(model=mdl,type='penalty',category=nmctg,name=nmp,level=nmlev,wgt=lev$wgt,nll=lev$pen,objfun=lev$objfun);
                            dfr<-rbind(dfr,rw);
                        }#level!=NULL
                    }#level
                }#penalty!=NULL         
            }#penalties
        }#categories
        mdfr<-reshape2::melt(dfr,id.vars=c('model','type','category','name','level'),measure.vars=c('wgt','nll','objfun'))
    } else if (class(repObj)=='list'){
        #repObj is a list of tcsam2015 model results objects
        mdls<-names(repObj);
        mdfr<-NULL;
        for (mdl in mdls){
            mdfr<-rbind(mdfr,getObjFunValues.Penalties(repObj[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getPenalties(repObj).\n")
        cat("'repObj' should be an object of class 'tcsam2015.rep' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(mdfr)
}
#mdfr.pens.1<-getObjFunValues.Penalties(repObj)
#mdfr.pens.2<-getObjFunValues.Penalties(list(base=repObj,alt1=repObj))