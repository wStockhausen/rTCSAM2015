#'
#'@title Plot model-simulated and observed data for comparison.
#'
#'@param res - results list object from report file
#'
#'@export
#'
plotDataModelComparisons<-function(res){
    #plot comparisons of abundance, biomass, and size frequencies
    #for model and data components
    
    #plot comparisons for survey abundance and biomass
    nSrv<-res$mc$nSrv;
    for (s in 1:nSrv){
        obs<-res$data$surveys[[s]];
        mod<-res$sim.data$surveys[[s]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        plotCatchData(name,obs,mod,label="Survey Catch");
    }
    
    #plot comparisons for fishery catches
    nFsh<-res$mc$nFsh;
    for (f in 1:nFsh){
        obs<-res$data$fisheries[[f]];
        mod<-res$sim.data$fisheries[[f]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        if (!is.null(mod$retained.catch)){
            plotCatchData(name,obs$retained.catch,mod$retained.catch,label="Retained Catch");
        }
        if (!is.null(mod$discard.catch)){
            plotCatchData(name,obs$discard.catch,mod$discard.catch,label="Discard Catch");
        }
        if (!is.null(mod$total.catch)){
            plotCatchData(name,obs$total.catch,mod$total.catch,label="Total Catch");
        }
    }
}


