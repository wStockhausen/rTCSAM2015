#'
#'@title Plot model-estimated population quantities such as natural mortality rates, maturity schedules, etc.
#'
#'@description Function to plot model-estimated population quantities such as 
#'natural mortality rates, maturity schedules, etc.
#'
#'@param repObj - model report list from TCSAM 2015 model
#'@param showPlot - flag to show plots immediately
#'
#'@return list of list of ggplot2 objects
#'
#'@export
#'
plotPopQuants<-function(repObj,showPlot=TRUE){
    
    plots<-list();
    
    #plot natural mortality estimates by parameter combination
    cat("plotting estimated natural mortality\n")
    plotNM.ByPC(repObj);
    
    cat("pplotting maturity schedules.\n")
    plotPrMaturity.ByPC(repObj);
    
    cat("plotting growth transition matrices.\n")
    plotPrGrowth.ByPC(repObj);
    
    #plot initial size distribution
    plots$initialSizeComps<-plotSingleSizeCompGG(repObj=repObj,component='pop.quants',
                                              title='Initial Size Comps',showPlot=FALSE)
    if (showPlot) print(plots$initialSizeComps)
    
    #plot model recruitment time series
    cat("plotting recruits.\n")
    plotRecruits(repObj,ylab="recruitment (millions)");
    plotRecruits(repObj,logscale=TRUE,ylab="recruitment (millions) [ln-scale]");
    
    cat("plotting recruitment sex fraction time series.\n")
    plotRecruits.SexRatio(repObj);
    
    #TODO: plot recruitment size distributions
    cat("TODO: plotting recruitment size distributions.\n")
    
    #plot spawning biomass
    cat("plotting spawning biomass.\n")
    plotSpB(repObj);
    
    #plot population abundance components
    cat("plotting population abundance components\n")
    plotPopComponents.Abundance(repObj);
    
    #plot population biomass components
    cat("plotting population biomass components\n")
    plotPopComponents.Biomass(repObj);
    
    return(invisible(plots))
}