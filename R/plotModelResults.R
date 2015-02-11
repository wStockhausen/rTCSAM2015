#'
#'@title Plot TCSAM 2015 model output.
#'
#'@description Function to plot data and results from a TCSAM2015 model run.
#'
#'@param res     - list object based on sourcing a TCSAM2015 model report file. can be NULL.
#'@param resfile - report file from a TCSAM2015 model run to source. can be NULL.
#'@param ggtheme - a ggplot2 theme to use with ggplot2 plots
#'@param out.pdf - file for pdf output (optional)
#'@return results model object.
#'
#'@details If res is NULL, resfile will be sourced locally to provide res. If
#'resfile is also NULL, the user will be prompted to identify a 
#'model report file from which to source the results object. \cr 
#'
#'@import ggplot2
#'
#'@export
#'
plotModelResults<-function(res=NULL,
                           resfile=NULL,
                           ggtheme=theme_grey(),
                           out.pdf=NULL){
    if (is.null(res)){
        res<-getModelResults(resfile);
        if (is.null(res)){
            cat("Model results list is NULL. Aborting...\n")
            return(NULL);
        }
    }
    
    if (class(res)!="tcsam2015"){
        cat("Input object 'res' does not appear to be a TCSAM2015 model results object",
            "Aborting...\n",sep='\n')
        return(NULL);
    }
    
    if (!is.null(out.pdf)){
        pdf(file=out.pdf,width=8.5,height=5);
        old.par<-par(omi=c(0.25,0.25,0.25,0.25))
    }
    
    #plot model parameter estimates and priors (but not std's or MCMC posteriors)
    cat("----------------------------------------------\n")
    cat("plotting model parameters and priors.\n")
    plotParameters(res);
    
    #plot objective function components
    cat("----------------------------------------------\n")
    cat("plotting objective function values.\n")
    ps<-plotObjFunValues(res,variable='objfun',showPlots=FALSE);
    print(ps);
    
    #plot simulated and observed data
    cat("----------------------------------------------\n")
    cat("plotting simulated and observed data.\n")
    plotDataModelComparisons(res);
    
    #plot model fits (z-scores, size comps, nlls)
    cat("----------------------------------------------\n")
    cat("plotting model fits.\n")
    plotZScoresForAll(res);
    
    #plot population quantities
    cat("----------------------------------------------\n")
    cat("plotting population quantities\n")
    plotPopQuants(res);
    
    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities.\n")
    plotSelFcns.ByPC(res);
    
    #plot fisheries quantities
    cat("----------------------------------------------\n")
    cat("plotting fisheries results.\n")
    plotFisheriesResults(res);
    
    #TODO: plot surveys quantities
    cat("----------------------------------------------\n")
    cat("TODO: plotting surveys results.\n")
#    plotSurveysResults(res);

    #TODO: stock-recruit curve(s)
    cat("----------------------------------------------\n")
    cat("TODO: plotting stock-recruit curves.\n")
    
    #TODO: control rule/phase plane plots
    cat("----------------------------------------------\n")
    cat("TODO: control rule/phase plane plots.\n")
    
    if (!is.null(out.pdf)){dev.off();}
    
    return(invisible(res));
}

#plotModelResults(res,out.pdf='test.pdf');
