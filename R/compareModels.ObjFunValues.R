#'
#'@title Plot all objective function values.
#'
#'@description Function to plot all objective function values.
#'
#'@param repObjs - model report object or list of report objects
#'@param variable - name of variable to plot
#'@param ggtheme - a ggplot2 theme
#'@param showPlot - flag to show plots immediately
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return list of ggplot2 objects corresponding to penalties, priors, and data plots.
#'
#'@export
#'
compareModels.ObjFunValues<-function(repObjs,
                                     variable='objfun',
                                     ggtheme=ggplot2::theme_grey(),
                                     showPlot=TRUE,
                                     verbose=FALSE){
    plots<-list();
    if (verbose) cat("plotting objective function values for penalties.\n")
    mdfr<-getObjFunValues.Penalties(repObjs,verbose=verbose);
    plots$penalties<-plotObjFunValues.Penalties(mdfr,
                                                variable=variable,
                                                ggtheme=ggtheme,
                                                showPlot=showPlot,
                                                verbose=verbose);
    
    if (verbose) cat("plotting objective function values for priors.\n")
    mdfr<-getObjFunValues.Priors(repObjs,verbose=verbose);
    plots$priors<-plotObjFunValues.Priors(mdfr,
                                          variable=variable,
                                          ggtheme=ggtheme,
                                          showPlot=showPlot,
                                          verbose=verbose);
    
    if (verbose) cat("plotting objective function values for data.\n")
    mdfr<-getObjFunValues.Data(repObjs,verbose=verbose);
    plots$data<-plotObjFunValues.Data(mdfr,
                                      variable=variable,
                                      ggtheme=ggtheme,
                                      showPlot=showPlot,
                                      verbose=verbose);
    return(plots);
}

#plots<-compareModelResults.ObjFunValues(repObjs); print(plots);