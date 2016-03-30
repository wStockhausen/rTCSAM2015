#'
#'@title Plot fits to catch data (abundance, biomass, size freq.s) as z-scores.
#'
#'@description Function to plot fits to catch data (abundance, biomass, size freq.s) as z-scores.
#'
#'@param fit - fits list for given catch type
#'@param mc - model configuration list from tcsam2015 report object (rep$mc, if rep is the report object)
#'@param fleet - fleet label
#'@param type - catch type label corresponding to the 'fit' object
#'@param showPlot - flag (T/F) to show plots immediately
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
plotZScoresForCatchData<-function(fit,
                                  mc,
                                  fleet,
                                  type="total catch",
                                  showPlot=TRUE,
                                  verbose=FALSE){
    plots<-list();
    if (!is.null(fit$abundance)){
        if (verbose) cat("Plotting abundance zscores\n")
        afits<-fit$abundance$fits;
        plots$abund<-plotZScoresGG(afits,
                                   ylab='abundance',
                                   label=paste0(fleet,"\n",type," abundance"),
                                   showPlot=showPlot,
                                   verbose=verbose);
    }
    if (!is.null(fit$biomass)){
        if (verbose) cat("Plotting biomass zscores\n")
        afits<-fit$biomass$fits;
        plots$biom<-plotZScoresGG(afits,
                                  ylab='biomass',
                                  label=paste0(fleet,"\n",type," biomass"),
                                  showPlot=showPlot,
                                  verbose=verbose);
    }
    if (!is.null(fit$n.at.z)){
        if (verbose) cat("Plotting fits for size frequencies.\n")
        plots$zfs <-plotFitsGG.SizeComps(fit$n.at.z,
                                         mc,
                                         label=paste0(fleet,"\n",type),
                                         showPlot=showPlot,
                                         verbose=verbose);
        plots$mzfs<-plotFitsGG.MeanSizeComps(fit$n.at.z,
                                                 mc,
                                                 label=paste0(fleet,"\n",type),
                                                 showPlot=showPlot);
        
        if (verbose) cat("Plotting size frequencies\n")
        plots$zrs<-plotZScoresGG1.SizeFreqs(fit$n.at.z,
                                            mc,
                                            label=paste0(fleet,"\n",type),
                                            showPlot=showPlot,
                                            verbose=verbose);
        
        if (verbose) cat("Plotting ESSs for size frequencies.\n")
        plots$effn<-plotEffNsGG(fit$n.at.z,
                                mc,
                                label=paste0(fleet,"\n",type),
                                showPlot=showPlot,
                                verbose=verbose);
    }
    return(plots);
}
