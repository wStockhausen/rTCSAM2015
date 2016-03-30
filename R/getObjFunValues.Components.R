#'
#'@title Get model components in the objective function as a melted dataframe
#'
#'@description Function to get model components in the objective function as a melted dataframe.
#'
#'@param res - tcsam2015 model report object or list of such
#'@param mdl - name to associate with model results object
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
#'"model", "type", "category", "name", "level", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@export
#'
getObjFunValues.Components<-function(res,
                                     mdl=NULL,
                                     verbose=FALSE){
    if (class(res)=='tcsam2015.rep'){
        #res is a tcsam2015 model report object
        if (is.null(mdl)) mdl<-res$mc$configName;
        comps<-res$model.fits$components;
        nmctgs<-names(comps);#names of model categories for components
        dfr<-NULL;
        for (nmctg in nmctgs){
            if(verbose) cat("Processing priors for category",nmctg,'\n')
            ctg<-comps[[nmctg]];#model category object
            nmps<-names(ctg);   #names of elements in category
            for (nmp in nmps){
                if(verbose) cat("Processing element",nmp,'\n')
                elem<-ctg[[nmp]]; #element object
                if (!is.null(elem)){
                    nmlevs<-names(elem);#names of element levels
                    for (nmlev in nmlevs){
                        if(verbose) cat("\tProcessing level",nmlev,'\n')
                        lev<-elem[[nmlev]];
                        if (!is.null(lev)){
                            rw<-data.frame(model=mdl,type='prior',category=nmctg,name=nmp,level=nmlev,wgt=lev$wgt,nll=lev$nll,objfun=lev$objfun);
                            dfr<-rbind(dfr,rw);
                        }#level!=NULL
                    }#levels
                }#parameter!=NULL
            }#parameters
        }#categories
        mdfr<-reshape2::melt(dfr,id.vars=c('model','type','category','name','level'),measure.vars=c('wgt','nll','objfun'))
    } else if (class(res)=='list'){
        #res is a list of tcsam2015 model report objects
        mdls<-names(res);
        mdfr<-NULL;
        for (mdl in mdls){
            mdfr<-rbind(mdfr,getObjFunValues.Components(res[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getObjFunValues.Components(res).\n")
        cat("'res' should be an object of class 'tcsam2015.rep' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(mdfr)
}
#mdfr1<-getObjFunValues.Components(res.fitSD.all)
#mdfr2<-getObjFunValues.Components(list(base=res.fitSD.all,alt1=res.fitSD.all))