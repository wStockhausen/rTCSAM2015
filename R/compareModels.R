#'
#'@title Compare model output from TCSAM2015 and/or rsimTCSAM model runs.
#'
#'@description Function to compare model output from TCSAM2015 and/or rsimTCSAM model runs.
#'
#'@param tcsams - single TCSAM2015 model results object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
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
compareModels<-function(tcsams=NULL,
                        rsims=NULL,
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
    
    #extract report objects 
    if (!is.null(tcsams)){
        repObjs<-list();
        for (t in names(tcsams)){
            repObjs[[t]]<-tcsams[[t]]$repObj;
        }
    }

    if (TRUE){
        #plot model parameter estimates and std devs
        cat("----------------------------------------------\n")
        cat("Comparing parameter values/est.s for tcsams models\n")
        if (!is.null(tcsams)){
            plots$params<-compareModels.ParamEsts(tcsams,dp=0.01,fac=2,nc=3,nr=4,
                                                  pdf=NULL,showPlot=showPlot||!is.null(pdf));
        }
        
        
        
        #plot objective function components
        if (!is.null(tcsams)){
            cat("----------------------------------------------\n")
            cat("plotting TCSM2015 objective function values.\n")
            plots$objfun<-compareModels.ObjFunValues(repObjs,variable='objfun',showPlot=showPlot||!is.null(pdf));
        }
    }
    
    #plot population processes
    cat("----------------------------------------------\n")
    cat("plotting population processes\n")
    plots$pop.processes<-compareModels.PopProcesses(repObjs,rsims,showPlot=showPlot||!is.null(pdf));

    #plot population quantities
    cat("----------------------------------------------\n")
    cat("plotting population quantities\n")
    plots$pop.quants<-compareModels.PopQuants(repObjs,rsims,showPlot=showPlot||!is.null(pdf));

    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities.\n")
    plots$selfcns<-compareModels.SelFcns.ByPC(repObjs,rsims,showPlot=showPlot||!is.null(pdf));

    #plot fisheries quantities
    cat("----------------------------------------------\n")
    cat("plotting fisheries results.\n")
    plots$fisheries.results<-compareModels.FisheryResults(repObjs,rsims,showPlot=showPlot||!is.null(pdf));

    #plot surveys quantities
    cat("----------------------------------------------\n")
    cat("plotting surveys results.\n")
    plots$surveys.results<-compareModels.SurveyResults(repObjs,rsims,showPlot=showPlot||!is.null(pdf));

    return(invisible(plots));
}
