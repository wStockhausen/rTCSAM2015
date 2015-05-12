#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for surveys.
#'
#'@param repObj - model report list object
#'
#'@return list by survey of lists with ggplot objects
#'
#'@export
#'
plotZScoresForSurveys<-function(repObj,showPlot=FALSE){
    plots.srv<-list();
    srvs<-names(repObj$model.fits$surveys)
    for (srv in srvs){
        plots<-list();
        cat("Plotting fits for survey '",srv,"'.\n",sep='')
        sfit<-repObj$model.fits$surveys[[srv]];
        if (!is.null(sfit$abundance)){
            cat("Plotting zscores for abundance time series.\n")
            afits<-sfit$abundance$fits;
            plots$abund<-plotZScoresGG(afits,ylab='abundance',label=srv,showPlot=showPlot)
        }
        if (!is.null(sfit$biomass)){
            cat("Plotting zscores for biomass time series.\n")
            afits<-sfit$biomass$fits;
            plots$biom<-plotZScoresGG(afits,ylab='biomass',label=srv,showPlot=showPlot)
        }
        if (!is.null(sfit$n.at.z)){
            cat("Plotting fits for size frequencies.\n")
            plots$zfs<-plotFitsGG.SizeComps(sfit$n.at.z,repObj$mc,label=srv,showPlot=showPlot)
            cat("Plotting zscores for size frequencies.\n")
            plots$zrs<-plotZScoresGG.SizeFreqs(sfit$n.at.z,repObj$mc,label=srv,showPlot=showPlot)
            cat("Plotting ESSs for size frequencies.\n")
            plots$effn<-plotEffNsGG(sfit$n.at.z,repObj$mc,label=srv,showPlot=showPlot)
        }
        plots.srv[[srv]]<-plots;
    }
    return(invisible(plots.srv));
}
