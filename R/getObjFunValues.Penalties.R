#'
#'@title Get model penalties in the objective function as a melted dataframe.
#'
#'@description Function to get model penalties as a melted dataframe.
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
#'"model", "type", "category", "name", "level", "variable", and "value".
#'
#'The "variable" column indicates whether the "value" is a weight ('wgt'),
#'negative log-likelihood ('nll'), or objective function value ('objfun').
#'
#'@importFrom reshape2 melt
#'
#'@export
#'
getObjFunValues.Penalties<-function(res,mdl=NULL){
    if (class(res)=='tcsam2015'){
        #res is a tcsam2015 model results object
        if (is.null(mdl)) mdl<-res$mc$configName;
        penalties<-res$model.fits$penalties;
        nmctgs<-names(penalties);#names of model categories for penalties
        dfr<-NULL;
        for (nmctg in nmctgs){
            cat("Processing penalties for category",nmctg,'\n')
            ctg<-penalties[[nmctg]];#model category object
            nmps<-names(ctg);   #names of penalties in category
            for (nmp in nmps){
                cat("Processing penalty for",nmp,'\n')
                pen<-ctg[[nmp]];
                if (!is.null(pen)){
                    nmlevs<-names(pen);
                    for (nmlev in nmlevs){
                        cat("\tProcessing level",nmlev,'\n')
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
    } else if (class(res)=='list'){
        #res is a list of tcsam2015 model results objects
        mdls<-names(res);
        mdfr<-NULL;
        for (mdl in mdls){
            mdfr<-rbind(mdfr,getObjFunValues.Penalties(res[[mdl]],mdl=mdl));
        }
    } else {
        cat("Error in getPenalties(res).\n")
        cat("'res' should be an object of class 'tcsam2015' or a list of such.\n")
        cat("Returning NULL.\n")
        return(NULL);
    }
    return(mdfr)
}
mdfr1<-getObjFunValues.Penalties(res.fitSD.all)
mdfr2<-getObjFunValues.Penalties(list(base=res.fitSD.all,alt1=res.fitSD.all))