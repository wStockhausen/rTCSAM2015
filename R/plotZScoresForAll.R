#'
#'@title Plot model fits to data components
#'
#'@param repObj - model report list object
#'@param showPlot - flag to show plots immediately
#'
#'@return list of lists of ggplot objects
#'
#'@export
#'
plotZScoresForAll<-function(repObj,showPlot=FALSE){
    #plot z-scores for fits to survey abundance and biomass
    plots.srv<-plotZScoresForSurveys(repObj,showPlot=showPlot);
    
    #plot z-scores for fits to fishery catch abundance and biomass
    plots.fsh<-plotZScoresForFisheries(repObj,showPlot=showPlot);
    
    return(invisible(list(surveys=plots.srv,fisheries=plots.fsh)))
}