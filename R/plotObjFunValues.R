#'
#'@title Plot all objective function values.
#'
#'@description Function to plot all objective function values.
#'
#'@param res - model results object or list of results objects
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
plotObjFunValues<-function(res,
                           variable='objfun',
                           ggtheme=ggplot2::theme_grey(),
                           showPlot=FALSE){
    ps<-list();
    cat("plotting objective function values for penalties.\n")
    mdfr<-getObjFunValues.Penalties(res);
    ps$penalties<-plotObjFunValues.Penalties(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    cat("plotting objective function values for priors.\n")
    mdfr<-getObjFunValues.Priors(res);
    ps$priors<-plotObjFunValues.Priors(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    cat("plotting objective function values for data.\n")
    mdfr<-getObjFunValues.Data(res);
    ps$data<-plotObjFunValues.Data(mdfr,variable=variable,ggtheme=ggtheme,showPlot=showPlot);
    return(ps);
}

#ps<-plotObjFunValues(res); print(ps);