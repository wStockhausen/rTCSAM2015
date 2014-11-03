#'
#'@title Plot model-simulated and observed data for comparison.
#'
#'@param res - results list object from report file
#'
#'@export
#'
#TODO: add in size frequency plots
plotSimData<-function(res){
    #plot comparisons of abundance, biomass, and size frequencies
    #for model and data components
    
    #plot comparisons for fishery catches
    nFsh<-res$mc$nFsh;
    for (f in 1:nFsh){
        obs<-res$data$fisheries[[f]];
        mod<-res$sim.data$fisheries[[f]];
        if (!is.null(mod$retained.catch)){
            plotCatchData(mod$name,obs$retained.catch,mod$retained.catch,ylab="Retained Catch");
        }
        if (!is.null(mod$discard.catch)){
            plotCatchData(mod$name,obs$discard.catch,mod$discard.catch,ylab="Discard Catch");
        }
        if (!is.null(mod$total.catch)){
            plotCatchData(mod$name,obs$total.catch,mod$total.catch,ylab="Total Catch");
        }
    }
    
    #plot comparisons for survey abundance and biomass
    nSrv<-res$mc$nSrv;
    for (s in 1:nSrv){
        obs<-res$data$surveys[[s]];
        mod<-res$sim.data$surveys[[s]];
        plotCatchData(mod$name,obs,mod,ylab="Survey Catch");
    }
}


