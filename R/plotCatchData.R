#'
#'@title Plot catch data
#'
#'@description Plot catch data (aggregate catch, catch-at-size) from fisheries or surveys
#'
#'@param name - fishery or survey name
#'@param obs - list of observed data 
#'@param mod - list of model-predicted data
#'@param logscale - flag (T/F) to plot on ln-scale
#'@param ylab - base title for y axis
#'
#'@export
#'
plotCatchData<-function(name=NULL,
                         obs=NULL,
                         mod=NULL,
                         logscale=FALSE,
                         ylab=""){
    #plot abundance
    if (!is.null(mod$abundance)){
        od<-obs$abundance;
        md<-mod$abundance;
        plotAggregateCatchData(name,od,md,ylab=ylab);
        plotAggregateCatchData(name,od,md,ylab=ylab,logscale=TRUE);
    }
    #plot biomass
    if (!is.null(mod$biomass)){
        od<-obs$biomass;
        md<-mod$biomass;
        plotAggregateCatchData(name,od,md,ylab=ylab);
        plotAggregateCatchData(name,od,md,ylab=ylab,logscale=TRUE);
    }
    #plot size frequencies
    if (!is.null(mod$nAtZ)){
        plotSizeCompsComparisons(name,od,md,ylab=)
    }
}
