#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for surveys.
#'
#'@param res - model results list object
#'
#'@export
#'
plotZScoresForSurveys<-function(res){
    srvs<-names(res$model.fits$surveys)
    for (srv in srvs){
        cat("Plotting fits for survey '",srv,"'.\n",sep='')
        sfit<-res$model.fits$surveys[[srv]];
        if (!is.null(sfit$abundance)){
            cat("Plotting zscores for abundance time series.\n")
            afits<-sfit$abundance$fits;
            plotZScoresGG(afits,ylab='abundance',label=srv)
        }
        if (!is.null(sfit$biomass)){
            cat("Plotting zscores for biomass time series.\n")
            afits<-sfit$biomass$fits;
            plotZScoresGG(afits,ylab='biomass',label=srv)
        }
        if (!is.null(sfit$n.at.z)){
            cat("Plotting zscores for size frequencies.\n")
            plotZScoresGG.SizeFreqs(sfit$n.at.z,res$mc,label=srv)
            cat("Plotting ESSs for size frequencies.\n")
            plotEffNsGG(sfit$n.at.z,res$mc,label=srv)
        }
    }
}
