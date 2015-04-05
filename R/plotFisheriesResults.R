#'
#'@title Plot model results for fisheries.
#'
#'@description Function to plot model results for fisheries.
#'
#'@param repObj - report list from a TCSAM2015 model run
#'@param showPlot - flag (T/F) to show plots
#'
#'@return nested list of ggplot objects
#'
#'@export
#'
plotFisheriesResults<-function(repObj,
                               showPlot=FALSE){
    plots<-list();
    #plot fishing rates
    cat("plotting fishing rates.\n")
    plots$fisheries.rates<-plotFishingRatesGG(repObj,showPlot=showPlot);
    
    #plot fishery catches
    cat("plotting fishery catches.\n")
    plots$fisheries.catch<-plotFisheryCatchesGG(repObj,showPlot=showPlot);
    
    #plot fishery catches
    cat("plotting fishery yields.\n")
    plots$fisheries.yield<-plotFisheryYieldsGG(repObj,showPlot=showPlot);
    
    #plot exploitation rates
    cat("plotting exploitation rates.\n")
    plots$fisheries.explrates<-plotExploitationRatesGG(repObj,showPlot=showPlot);
    
    return(invisible(plots))
}

#plots<-plotFisheriesResults(repObj)