#'
#'@title Get model priors in the objective function as a melted dataframe
#'
#'@description Function to get model priors as a melted dataframe.
#'
#'@param repObjs - tcsam2015 model report object or list of such
#'@param mdl - name to associate with model results object
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
#'"model", "type", "category", "name", "level", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getObjFunValues.Priors<-function(repObjs,
                                 mdl=NULL,
                                 verbose=FALSE){
    if (inherits(repObjs,'tcsam2015.rep')){
        #repObjs is a tcsam2015 model results object
        if (is.null(mdl)) mdl<-repObjs$mc$configName;
        priors<-repObjs$model.fits$priors;
        nmctgs<-names(priors);#names of model categories for priors
        dfr<-NULL;
        for (nmctg in nmctgs){
            if(verbose) cat("Processing priors for category",nmctg,'\n')
            ctg<-priors[[nmctg]];#model category object
            nmps<-names(ctg);   #names of parameters in category
            for (nmp in nmps){
                if(verbose) cat("Processing priors for parameter",nmp,'\n')
                param<-ctg[[nmp]]; #model parameter object
                if (!is.null(param)){
                    nmlevs<-names(param);#names of parameter levels
                    for (nmlev in nmlevs){
                        if(verbose) cat("\tProcessing level",nmlev,'\n')
                        lev<-param[[nmlev]];
                        if (!is.null(lev)){
                            rw<-data.frame(model=mdl,type='prior',category=nmctg,name=nmp,level=nmlev,wgt=lev$wgt,nll=lev$nll,objfun=lev$objfun);
                            dfr<-rbind(dfr,rw);
                        }#level!=NULL
                    }#levels
                }#parameter!=NULL
            }#parameters
        }#categories
        mdfr<-reshape2::melt(dfr,id.vars=c('model','type','category','name','level'),measure.vars=c('wgt','nll','objfun'))
    } else if (class(repObjs)=='list'){
        #repObjs is a list of tcsam2015 model report objects
        mdls<-names(repObjs);
        mdfr<-NULL;
        for (mdl in mdls){
            mdfr<-rbind(mdfr,getObjFunValues.Priors(repObjs[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getPriors(repObjs).\n")
        cat("'repObjs' should be an object of class 'tcsam2015.rep' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(mdfr)
}
#mdfr.priors.1<-getObjFunValues.Priors(repObjs)
#mdfr.priors.2<-getObjFunValues.Priors(list(base=repObjs,alt1=repObjs))