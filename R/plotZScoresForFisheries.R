#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for fishery data components.
#'
#'@param repObj - model report list object
#'
#'@return list by fishery of lists with ggplot objects
#'
#'@export
#'
plotZScoresForFisheries<-function(repObj,showPlot=FALSE){
    plots.fsh<-list();
    fshs<-names(repObj$model.fits$fisheries)
    for (fsh in fshs){
        plots.ret<-list();
        fit<-repObj$model.fits$fisheries[[fsh]]$retained.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting retained catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plots.ret$abund<-plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","retained catch abundance",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$biomass)){
                cat("Plotting retained catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plots.ret$biom<-plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","retained catch biomass",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting fits for total catch size frequencies.\n")
                plots.ret$zfs<-plotFitsGG.SizeComps(fit$n.at.z,repObj$mc,label=paste(fsh,": ","retained catch",sep=''),showPlot=showPlot)
                cat("Plotting retained catch size frequency zscores for",fsh,"\n")
                plots.ret$zrs<-plotZScoresGG.SizeFreqs(fit$n.at.z,repObj$mc,label=paste(fsh,": ","retained catch",sep=''))
                cat("Plotting ESSs for size frequencies.\n")
                plots.ret$effn<-plotEffNsGG(fit$n.at.z,repObj$mc,label=paste(fsh,": ","retained catch",sep=''),showPlot=showPlot)
            }
        }
        plots.dsc<-list();
        fit<-repObj$model.fits$fisheries[[fsh]]$discard.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting discard catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plots.dsc$abund<-plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","discard catch abundance",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$biomass)){
                cat("PLotting discard catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plots.dsc$biom<-plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","discard catch biomass",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting fits for discard catch size frequencies.\n")
                plots.tot$zfs<-plotFitsGG.SizeComps(fit$n.at.z,repObj$mc,label=paste(fsh,": ","discard catch",sep=''),showPlot=showPlot)
                cat("Plotting discard catch size frequency zscores for",fsh,"\n")
                plots.dsc$zrs<-plotZScoresGG.SizeFreqs(fit$n.at.z,repObj$mc,label=paste(fsh,": ","discard catch",sep=''),showPlot=showPlot)
                cat("Plotting ESSs for size frequencies.\n")
                plots.dsc$effn<-plotEffNsGG(fit$n.at.z,repObj$mc,label=paste(fsh,": ","discard catch",sep=''),showPlot=showPlot)
            }
        }
        plots.tot<-list();
        fit<-repObj$model.fits$fisheries[[fsh]]$total.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting total catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plots.tot$abund<-plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","total catch abundance",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$biomass)){
                cat("Plotting total catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plots.tot$biom<-plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","total catch biomass",sep=''),showPlot=showPlot);
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting fits for total catch size frequencies.\n")
                plots.tot$zfs<-plotFitsGG.SizeComps(fit$n.at.z,repObj$mc,label=paste(fsh,": ","total catch",sep=''),showPlot=showPlot)
                cat("Plotting total catch size frequencies for",fsh,"\n")
                plots.tot$zrs<-plotZScoresGG.SizeFreqs(fit$n.at.z,repObj$mc,label=paste(fsh,": ","total catch",sep=''),showPlot=showPlot)
                cat("Plotting ESSs for size frequencies.\n")
                plots.tot$effn<-plotEffNsGG(fit$n.at.z,repObj$mc,label=paste(fsh,": ","total catch",sep=''),showPlot=showPlot)
            }
        }
        plots.fsh[[fsh]]<-list(ret=plots.ret,dsc=plots.dsc,tot=plots.tot);
    }#fsh
    return(invisible(plots.fsh));
}
