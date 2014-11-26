#'
#'@title Plot TCSAM 2015 model output.
#'
#'@description Function to plot data and results from a TCSAM2015 model run.
#'
#'@param res     - list object based on sourcing a TCSAM2015 model report file. can be NULL.
#'@param resfile - report file from a TCSAM2015 model run to source. can be NULL.
#'@param out.pdf - file for pdf output (optional)
#'@return results model object.
#'
#'@details If res is NULL, resfile will be sourced locally to provide res. If
#'resfile is also NULL, the user will be prompted to identify a 
#'model report file from which to source the results object. \cr 
#'
#'@export
#'
plotModelResults<-function(res=NULL,
                           resfile=NULL,
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
    
    #plot simulated and observed data
    cat("plotting simulated and observed data.\n")
    plotDataModelComparisons(res);
    
    #plot model fits (z-scores, size comps, nlls)
    cat("plotting model fits.\n")
    plotZScoresForAll(res);
    
    #plot model parameter estimates and priors (but not std's or MCMC posteriors)
    cat("plotting model parameters and priors.\n")
    plotParameters(res);
    
    #plot population quantities
    cat("plotting population quantities\n")
    plotPopQuants(res);
    
    #plot selectivities
    cat("plotting selectivities.\n")
    plotSelFcns.ByPC(res);
    
    #plot fisheries quantities
    cat("plotting fisheries results.\n")
    plotFisheriesResults(res);
    
    #TODO: plot surveys quantities
    cat("TODO: plotting surveys results.\n")
#    plotSurveysResults(res);

    #TODO: stock-recruit curve(s)
    cat("TODO: plotting stock-recruit curves.\n")
    
    #TODO: control rule/phase plane plots
    cat("TODO: control rule/phase plane plots.\n")
    
    if (!is.null(out.pdf)){dev.off();}
    
    return(invisible(res));
}