#'
#'@title Compare fisheries-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare fisheries-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModels.FisheryResults<-function(tcsam=NULL,
                                  rsim=NULL,
                                  showPlot=TRUE,
                                  pdf=NULL,
                                  width=8,
                                  height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=out.pdf,width=width,height=height);
        on.exit(dev.close())
    }
    
    plots<-list();
    
    #check handling mortality/fishery equations
    p<-checkHandlingMortalityConsistency(tcsam,rsim,showPlot=FALSE);
    if (showPlot) print(p);
    plots$hmCheck<-p;
    
    #fishing rates
    p<-compareModels.FishingRates(tcsam,rsim,showPlot=FALSE)
    if (showPlot) print(p);
    plots$fishingRates<-p;
    
    #fishery catches (abundance)
    p<-compareModels.FisheryCatches(tcsam,rsim,showPlot=FALSE)
    if (showPlot) print(p);
    plots$fisheryCatches<-p;
    
    #fishery yields (biomass)
    p<-compareModels.FisheryYields(tcsam,rsim,showPlot=FALSE)
    if (showPlot) print(p);
    plots$fisheryYields<-p;
    
    return(invisible(plots))
}