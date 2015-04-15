#'
#'@title Compare model output from TCSAM2015 and/or rsimTCSAM model runs.
#'
#'@description Function to compare model output from TCSAM2015 and/or rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param ggtheme - a ggplot2 theme to use with ggplot2 plots
#'@param showPlot - flag to show plots immediately
#'@param pdf - filename for pdf output (optional)
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return multi-level list of ggplot2 objects
#'
#'@import ggplot2
#'
#'@export
#'
compareModels<-function(tcsam=NULL,
                        rsim=NULL,
                        ggtheme=theme_grey(),
                        showPlot=TRUE,
                        pdf=NULL,
                        width=8,
                        height=6){
    
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        old.par<-par(omi=c(0.25,0.25,0.25,0.25))
        on.exit(dev.off());
        on.exit(par(old.par),add=TRUE);
    }
    
    plots<-list();
    
    #plot model parameter estimates and std devs
    cat("----------------------------------------------\n")
    cat("TODO: need to be able to compare parameter values/est.s between rsim, TCSAM models!!\n")
    
    #plot objective function components
    if (!is.null(tcsam)){
        cat("----------------------------------------------\n")
        cat("plotting TCSM2015 objective function values.\n")
        plots$objfun<-compareModels.ObjFunValues(tcsam,variable='objfun',showPlot=FALSE);
        if (showPlot) print(plots$objfun);
    }
    
    #plot population quantities
    cat("----------------------------------------------\n")
    cat("plotting population quantities\n")
    plots$pop.quants<-compareModels.PopQuants(tcsam,rsim,showPlot=FALSE);
    if (showPlot) print(plots$pop.quants);
    
    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities.\n")
    plots$selfcns<-compareModels.SelFcns.ByPC(tcsam,rsim,showPlot=FALSE);
    if (showPlot) print(plots$selfcns);
    
    #plot fisheries quantities
    cat("----------------------------------------------\n")
    cat("plotting fisheries results.\n")
    plots$fisheries.results<-compareModels.FisheryResults(tcsam,rsim,showPlot=FALSE);
    if (showPlot) print(plots$fisheries.results);
    
    #plot surveys quantities
    cat("----------------------------------------------\n")
    cat("plotting surveys results.\n")
    plots$surveys.results<-compareModels.SurveyResults(tcsam,rsim,showPlot=FALSE);
    if (showPlot) print(plots$surveys.results);
    
    return(invisible(plots));
}

#plotModelrepObjults(repObj,out.pdf='test.pdf');
