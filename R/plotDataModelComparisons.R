#'
#'@title Plot model-estimated and observed data for comparison.
#'
#'@param repObj - results list object from report file
#'@param showPlot - flag to show plot immediately
#'
#'@return multi-level list with ggplot2 objects
#'
#'@export
#'
plotDataModelComparisons<-function(repObj,showPlot=FALSE){
    #plot comparisons of abundance, biomass, and size frequencies
    #for model and data components
    
    #plot comparisons for survey abundance and biomass
    plots.srv<-list();
    nSrv<-repObj$mc$dims$v$n;
    for (s in 1:nSrv){
        obs<-repObj$data$surveys[[s]];
        mod<-repObj$sim.data$surveys[[s]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        plots.srv[[s]]<-plotCatchData(name,obs,mod,label="Survey Catch",showPlot=showPlot);
    }
    
    #plot comparisons for fishery catches
    plots.fsh<-list();
    nFsh<-repObj$mc$dims$f$n;
    for (f in 1:nFsh){
        plots.fsh[[f]]<-list();
        obs<-repObj$data$fisheries[[f]];
        mod<-repObj$sim.data$fisheries[[f]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        if (!is.null(mod$retained.catch)){
            plots.fsh[[f]]$ret<-plotCatchData(name,obs$retained.catch,mod$retained.catch,label="Retained Catch",showPlot=showPlot);
        }
        if (!is.null(mod$discard.catch)){
            plots.fsh[[f]]$dsc<-plotCatchData(name,obs$discard.catch,mod$discard.catch,label="Discard Catch",showPlot=showPlot);
        }
        if (!is.null(mod$total.catch)){
            plots.fsh[[f]]$tot<-plotCatchData(name,obs$total.catch,mod$total.catch,label="Total Catch",showPlot=showPlot);
        }
    }
    return(invisible(list(surveys=plots.srv,fisheries=plots.fsh)));
}


