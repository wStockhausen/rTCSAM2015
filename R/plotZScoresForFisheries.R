#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for fishery data components.
#'
#'@param res - model results list object
#'
#'@export
#'
plotZScoresForFisheries<-function(res){
    fshs<-names(res$model.fits$fisheries)
    for (fsh in fshs){
        fit<-res$model.fits$fisheries[[fsh]]$retained.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting retained catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","retained catch abundance",sep=''));
            }
            if (!is.null(fit$biomass)){
                cat("Plotting retained catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","retained catch biomass",sep=''));
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting retained catch size frequency zscores for",fsh,"\n")
                plotZScoresGG.SizeFreqs(fit$n.at.z,res$mc,label=paste(fsh,": ","retained catch",sep=''))
                cat("Plotting ESSs for size frequencies.\n")
                plotEffNsGG(fit$n.at.z,res$mc,label=paste(fsh,": ","retained catch",sep=''))
            }
        }
        fit<-res$model.fits$fisheries[[fsh]]$discard.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting discard catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","discard catch abundance",sep=''));
            }
            if (!is.null(fit$biomass)){
                cat("PLotting discard catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","discard catch biomass",sep=''));
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting discard catch size frequency zscores for",fsh,"\n")
                plotZScoresGG.SizeFreqs(fit$n.at.z,res$mc,label=paste(fsh,": ","discard catch",sep=''))
                cat("Plotting ESSs for size frequencies.\n")
                plotEffNsGG(fit$n.at.z,res$mc,label=paste(fsh,": ","discard catch",sep=''))
            }
        }
        fit<-res$model.fits$fisheries[[fsh]]$total.catch;
        if (!is.null(fit)){
            if (!is.null(fit$abundance)){
                cat("Plotting total catch abundance zscores for",fsh,"\n")
                afits<-fit$abundance$fits;
                plotZScoresGG(afits,ylab='abundance',label=paste(fsh,": ","total catch abundance",sep=''));
            }
            if (!is.null(fit$biomass)){
                cat("Plotting total catch biomass zscores for",fsh,"\n")
                afits<-fit$biomass$fits;
                plotZScoresGG(afits,ylab='biomass',label=paste(fsh,": ","total catch biomass",sep=''));
            }
            if (!is.null(fit$n.at.z)){
                cat("Plotting total catch size frequencies for",fsh,"\n")
                plotZScoresGG.SizeFreqs(fit$n.at.z,res$mc,label=paste(fsh,": ","total catch",sep=''))
                cat("Plotting ESSs for size frequencies.\n")
                plotEffNsGG(fit$n.at.z,res$mc,label=paste(fsh,": ","total catch",sep=''))
            }
        }
    }
}
