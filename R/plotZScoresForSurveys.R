#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for surveys.
#'
#'@param res - model results list object
#'
#'@return list by survey of lists with ggplot objects
#'
#'@export
#'
plotZScoresForSurveys<-function(res,showPlot=FALSE){
    plots.srv<-list();
    srvs<-names(res$model.fits$surveys)
    for (srv in srvs){
        plots<-list();
        cat("Plotting fits for survey '",srv,"'.\n",sep='')
        sfit<-res$model.fits$surveys[[srv]];
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
            cat("Plotting zscores for size frequencies.\n")
            plots$zfs<-plotZScoresGG.SizeFreqs(sfit$n.at.z,res$mc,label=srv,showPlot=showPlot)
            cat("Plotting ESSs for size frequencies.\n")
            plots$effn<-plotEffNsGG(sfit$n.at.z,res$mc,label=srv,showPlot=showPlot)
        }
        plots.srv[[srv]]<-plots;
    }
    return(invisible(plots.srv));
}
