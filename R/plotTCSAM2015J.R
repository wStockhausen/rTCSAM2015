#'
#'@title Plot TCSAM2015 model output
#'
#'@description Function to plot data and results from a TCSAM2015 model run.
#'
#'@param repObj - tcsam2015.rep object based on sourcing a TCSAM2015 model report file. can be NULL.
#'@param prsObj - tcsam2015.prs object based on reading a TCSAM2015 active parameters csv file. can be NULL.
#'@param stdObj - tcsam2015.std object based on reading a TCSAM2015 std file. can be NULL.
#'@param objList - list with optional elements repObj, prsObj, stdObj (an optional way to provide the Obj's)
#'@param ggtheme - a ggplot2 theme to use with ggplot2 plots
#'@param showPlot - flag to show plots immediately
#'@param pdf - filename for pdf output (optional)
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return  multi-level list of ggplot2 objects
#'
#'@details none
#'
#'@export
#'
plotTCSAM2015J<-function(repObj=NULL,
                            prsObj=NULL,
                            stdObj=NULL,
                            objList=NULL,
                            ggtheme=theme_grey(),
                            showPlot=TRUE,
                            pdf=NULL,
                            width=14,
                            height=8,
                            verbose=FALSE){
    
    if (!is.null(objList)){
        repObj<-objList$repObj;
        prsObj<-objList$prsObj;
        stdObj<-objList$stdObj;
    }
    
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        old.par<-par(omi=c(0.25,0.25,0.25,0.25))
        on.exit(dev.off());
        on.exit(par(old.par),add=TRUE);
    }
    
    plots<-list();
    
    #plot data time frames
    cat("----------------------------------------------\n")
    cat("plotting input data time frames.\n")
    plots$datatimeframes<-plotInputDataTimeframes(repObj,showPlot=showPlot);
    
    if (!is.null(prsObj)){
        #plot model parameter estimates and std devs (but not priors)
        cat("----------------------------------------------\n")
        cat("plotting model parameters and std devs.\n")
        resLst<-compareModels.ParamEsts(list(tcsam=list(prsObj=prsObj,stdObj=stdObj)),
                                        showPlot=showPlot,pdf=NULL,
                                        verbose=verbose);
        plots$params<-resLst$plots;
        #if (showPlot) {for (p in plots$params) print(p);}
    }
    
    #plot model parameter estimates and priors (but not std's or MCMC posteriors)
    cat("----------------------------------------------\n")
    cat("plotting model parameters and priors.\n")
    plotParameters(repObj,verbose=verbose);
    
    #plot objective function components
    cat("----------------------------------------------\n")
    cat("plotting objective function values.\n")
    plots$objfun<-compareModels.ObjFunValues(repObj,variable='objfun',
                                             showPlot=showPlot,verbose=verbose);
    #if (showPlot) {for (p in plots$objfun) print(p);}
    
    #plot model fits (z-scores, size comps, nlls)
    cat("----------------------------------------------\n")
    cat("plotting model fits.\n")
    plots$zscores<-plotZScoresForAll(repObj,
                                     showPlot=showPlot,
                                     verbose=verbose);
    #if (showPlot) print(plots$zscores);
    
    #plot population processes
    cat("----------------------------------------------\n")
    cat("plotting population processes\n")
    plots$pop.quants<-compareModels.PopProcesses(repObj,showPlot=showPlot);
    #if (showPlot) print(plots$pop.quants);
    
    #plot fisheries quantities
    cat("----------------------------------------------\n")
    cat("plotting fisheries results.\n")
    plots$fisheries.results<-compareModels.FisheryResults(repObj,showPlot=showPlot);
    #if (showPlot) print(plots$fisheries.results);
    
    #plot surveys quantities
    cat("----------------------------------------------\n")
    cat("plotting surveys results.\n")
    plots$surveys.results<-compareModels.SurveyResults(repObj,showPlot=showPlot);
    #if (showPlot) print(plots$surveys.results);
    
    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities by parameter group.\n")
    plots$selfcns.byPC<-compareModels.SelFcns.ByPC(repObj,showPlot=showPlot);

    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities by year.\n")
    plots$selfcns.byYr<-plotSelFcns.ByYear(repObj,showPlot=showPlot);

    #plot effort regressions
    cat("----------------------------------------------\n")
    cat("plotting effort regressions.\n");
    plots$effRegs<-plotEffortRegressions(repObj,showPlot=showPlot);

    return(invisible(plots));
}

#plotModelResultsI(repObj,pdf='test.pdf');
