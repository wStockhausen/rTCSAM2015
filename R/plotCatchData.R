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
#'
#'@export
#'
plotCatchData<-function(name=NULL,
                        obs=NULL,
                        mod=NULL,
                        logscale=FALSE,
                        normalize=TRUE,
                        label=""){
    #plot abundance
    if (!is.null(mod$abundance)){
        od<-obs$abundance;
        md<-mod$abundance;
        plotAggregateCatchData(name,od,md,ylab=label);
        plotAggregateCatchData(name,od,md,ylab=label,logscale=TRUE);
    }
    #plot biomass
    if (!is.null(mod$biomass)){
        od<-obs$biomass;
        md<-mod$biomass;
        plotAggregateCatchData(name,od,md,ylab=label);
        plotAggregateCatchData(name,od,md,ylab=label,logscale=TRUE);
    }
    #plot size frequencies
    if (!is.null(mod$nAtZ)){
        cat('name  =',name,'\n')
        od<-obs$nAtZ;
        md<-mod$nAtZ;
#         ps0<-plotSizeCompsComparisons0(name,od,md,normalize=normalize,label=label,showPlots=FALSE);
#         print(ps0);
#         ps1<-plotSizeCompsComparisons1(name,od,md,normalize=normalize,label=label,showPlots=FALSE)
#         print(ps1);
        ps2<-plotSizeCompsComparisons2(name,od,md,normalize=normalize,label=label,showPlots=FALSE)
        print(ps2);
        psM<-plotMeanSizeCompsGG(name,od,md,normalize=normalize,label=label,showPlots=FALSE);
        print(psM);
    }
}
