#'
#'@title Plot model-estimated and observed data for comparison.
#'
#'@param res - results list object from report file
#'@param showPlot - flag to show plot immediately
#'
#'@return multi-level list with ggplot2 objects
#'
#'@export
#'
plotDataModelComparisons<-function(res,showPlot=FALSE){
    #plot comparisons of abundance, biomass, and size frequencies
    #for model and data components
    
    #plot comparisons for survey abundance and biomass
    ps.srv<-list();
    nSrv<-res$mc$nSrv;
    for (s in 1:nSrv){
        obs<-res$data$surveys[[s]];
        mod<-res$sim.data$surveys[[s]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        ps.srv[[s]]<-plotCatchData(name,obs,mod,label="Survey Catch",showPlot=showPlot);
    }
    
    #plot comparisons for fishery catches
    ps.fsh<-list();
    nFsh<-res$mc$nFsh;
    for (f in 1:nFsh){
        ps.fsh[[f]]<-list();
        obs<-res$data$fisheries[[f]];
        mod<-res$sim.data$fisheries[[f]];
        name<-gsub('.',' ',mod$name,fixed=TRUE);
        if (!is.null(mod$retained.catch)){
            ps.fsh[[f]]$ret<-plotCatchData(name,obs$retained.catch,mod$retained.catch,label="Retained Catch",showPlot=showPlot);
        }
        if (!is.null(mod$discard.catch)){
            ps.fsh[[f]]$dsc<-plotCatchData(name,obs$discard.catch,mod$discard.catch,label="Discard Catch",showPlot=showPlot);
        }
        if (!is.null(mod$total.catch)){
            ps.fsh[[f]]$tot<-plotCatchData(name,obs$total.catch,mod$total.catch,label="Total Catch",showPlot=showPlot);
        }
    }
    return(list(surveys=ps.srv,fisheries=ps.fsh));
}


