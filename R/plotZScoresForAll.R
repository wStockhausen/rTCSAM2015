#'
#'@title Plot model fits to data components
#'
#'@param res - model results list object
#'
#'@export
#'
plotZScoresForAll<-function(res){
    #plot z-scores for fits to survey abundance and biomass
    plotZScoresForSurveys(res);
    
    #plot z-scores for fits to fishery catch abundance and biomass
    plotZScoresForFisheries(res);
}