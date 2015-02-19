#'
#'@title Plot model fits to data components
#'
#'@param res - model results list object
#'@param showPlot - flag to show plots immediately
#'
#'@return list of lists of ggplot objects
#'
#'@export
#'
plotZScoresForAll<-function(res,showPlot=FALSE){
    #plot z-scores for fits to survey abundance and biomass
    ps.srv<-plotZScoresForSurveys(res,showPlot=showPlot);
    
    #plot z-scores for fits to fishery catch abundance and biomass
    ps.fsh<-plotZScoresForFisheries(res,showPlot=showPlot);
    
    return(list(surveys=ps.srv,fisheries=ps.fsh))
}