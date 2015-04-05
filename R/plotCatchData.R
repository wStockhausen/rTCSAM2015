#'
#'@title Plot catch data
#'
#'@description Plot catch data (aggregate catch, catch-at-size) from fisheries or surveys
#'
#'@param name - fishery or survey name
#'@param obs - list of observed data 
#'@param mod - list of model-predicted data
#'@param logscale - flag (T/F) to plot on ln-scale
#'@param label - base label for plots
#'@param showPlot - flag to show plots immediately
#'
#'@return multi-level list of ggplot2 objects
#'
#'@export
#'
plotCatchData<-function(name=NULL,
                        obs=NULL,
                        mod=NULL,
                        logscale=FALSE,
                        normalize=TRUE,
                        label="",
                        showPlot=FALSE){
    plots<-list();
    #plot abundance
    if (!is.null(mod$abundance)){
        cat('Plotting abundance using plotAggregateCatchDataGG\n')
        plots$abundance<-list();
        od<-obs$abundance;
        md<-mod$abundance;
        plots$abundance$arscl<-plotAggregateCatchDataGG(name,od,md,pdfType=md$llType,ylab=label,showPlot=showPlot);
        plots$abundance$lnscl<-plotAggregateCatchDataGG(name,od,md,pdfType=md$llType,ylab=label,logscale=TRUE,showPlot=showPlot);
    }
    #plot biomass
    if (!is.null(mod$biomass)){
        cat('Plotting biomass using plotAggregateCatchDataGG\n')
        plots$biomass<-list();
        od<-obs$biomass;
        md<-mod$biomass;
        plots$biomass$arscl<-plotAggregateCatchDataGG(name,od,md,pdfType=md$llType,ylab=label,showPlot=showPlot);
        plots$biomass$lnscl<-plotAggregateCatchDataGG(name,od,md,pdfType=md$llType,ylab=label,logscale=TRUE,showPlot=showPlot);
    }
    #plot size frequencies
    if (!is.null(mod$nAtZ)){
        plots$zfs<-list();
        cat('name  =',name,'\n')
        od<-obs$nAtZ;
        md<-mod$nAtZ;
#         ps0<-plotSizeCompsComparisons0(name,od,md,normalize=normalize,label=label,showPlots=FALSE);
#         print(ps0);
#         ps1<-plotSizeCompsComparisons1(name,od,md,normalize=normalize,label=label,showPlots=FALSE)
#         print(ps1);
        plots$zfs$comps<-plotSizeCompsComparisons2(name,od,md,normalize=normalize,label=label,showPlot=showPlot)
        plots$zfs$mean<-plotMeanSizeCompsGG(name,od,md,normalize=normalize,label=label,showPlot=showPlot);
    }
    return(invisible(plots));
}
