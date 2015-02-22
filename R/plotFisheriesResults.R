#'
#'@title Plot model results for fisheries.
#'
#'@description Function to plot model results for fisheries.
#'
#'@param res - results list from a TCSAM2015 model run
#'@param showPlot - flag (T/F) to show plots
#'
#'@return nested list of ggplot objects
#'
#'@export
#'
plotFisheriesResults<-function(res,
                               showPlot=FALSE){
    ps<-list();
    #plot fishing rates
    cat("plotting fishing rates.\n")
    ps$fisheries.rates<-plotFishingRatesGG(res,showPlot=showPlot);
    
    #plot fishery catches
    cat("plotting fishery catches.\n")
    ps$fisheries.catch<-plotFisheryCatchesGG(res,showPlot=showPlot);
    
    #plot fishery catches
    cat("plotting fishery yields.\n")
    ps$fisheries.yield<-plotFisheryYieldsGG(res,showPlot=showPlot);
    
    #plot exploitation rates
    cat("plotting exploitation rates.\n")
    ps$fisheries.explrates<-plotExploitationRatesGG(res,showPlot=showPlot);
    
    return(invisible(ps))
}

#ps<-plotFisheriesResults(res)