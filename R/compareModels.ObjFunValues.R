#'
#'@title Plot all objective function values.
#'
#'@description Function to plot all objective function values.
#'
#'@param repObj - model report object or list of report objects
#'@param variable - name of variable to plot
#'@param ggtheme - a ggplot2 theme
#'@param showPlot - flag to show plots immediately
#'
#'@return list of ggplot2 objects corresponding to penalties, priors, and data plots.
#'
#'@importFrom ggplot2 theme_grey
#'
#'@export
#'
compareModels.ObjFunValues<-function(repObj,
                                     variable='objfun',
                                     ggtheme=ggplot2::theme_grey(),
                                     showPlot=FALSE){
    plots<-list();
    cat("plotting objective function values for penalties.\n")
    mdfr<-getObjFunValues.Penalties(repObj);
    plots$penalties<-plotObjFunValues.Penalties(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    cat("plotting objective function values for priors.\n")
    mdfr<-getObjFunValues.Priors(repObj);
    plots$priors<-plotObjFunValues.Priors(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    cat("plotting objective function values for data.\n")
    mdfr<-getObjFunValues.Data(repObj);
    plots$data<-plotObjFunValues.Data(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    return(invisible(plots));
}

#plots<-compareModelResults.ObjFunValues(repObj); print(plots);