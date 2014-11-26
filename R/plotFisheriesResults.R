#'
#'@title Plot model results for fisheries.
#'
#'@description Function to plot model results for fisheries.
#'
#'@param res - results list from a TCSAM2015 model run
#'@param showPlots - flag (T/F) to show plots
#'
#'@return nested list of ggplot objects
#'
#'@export
#'
plotFisheriesResults<-function(res,
                               showPlots=TRUE){
    ps<-list();
    #plot fishing rates
    cat("plotting fishing rates.\n")
    ps$fisheries.rates<-plotFishingRatesGG(res,showPlots=showPlots);
    
    #plot fishery catches
    cat("plotting fishery catches.\n")
    ps$fisheries.catch<-plotFisheryCatchesGG(res,showPlots=showPlots);
    
    #plot fishery catches
    cat("plotting fishery yields.\n")
    ps$fisheries.yield<-plotFisheryYieldsGG(res,showPlots=showPlots);
    
    #plot exploitation rates
    cat("plotting exploitation rates.\n")
    ps$fisheries.yield<-plotExploitationRatesGG(res,showPlots=showPlots);
    
    return(ps)
}

#ps<-plotFisheriesResults(res)