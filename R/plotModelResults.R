#'
#'@title Plot TCSAM 2015 model output.
#'
#'@description Function to plot data and repObjults from a TCSAM2015 model run.
#'
#'@param repObj - list object based on sourcing a TCSAM2015 model report file. can be NULL or a filename.
#'@param prsObj - list object based on reading a TCSAM2015 active parameters csv file. can be NULL or a filename.
#'@param stdObj - list object based on reading a TCSAM2015 std file. can be NULL or a filename.
#'@param objList - list with optional elements repObj, prsObj, stdObj (an optional way to provide the Obj's)
#'@param ggtheme - a ggplot2 theme to use with ggplot2 plots
#'@param showPlot - flag to show plots immediately
#'@param pdf - filename for pdf output (optional)
#'
#'@return list with possible elements:\cr
#'* repObj - TCSAM2015 report object (if report file was read)
#'* prsObj - TCSAM2015 parameters dataframe (if parameters csv file was read)
#'* stdObj - TCSAM2015 std devs dataframe (if std file was read)
#'* plots: multi-level list of ggplot2 objects
#'
#'@details If repObj is a character string (the path to a report file), the file will be read in.
#'If repObj is NULL, the user will be prompted to identify a 
#'model report file from which to source the repObjults object. Similarly for prsObj and stdObj.
#'Any of these objects obtained by reading a file will be an element in the returned list.
#'
#'@import ggplot2
#'
#'@export
#'
plotModelResults<-function(repObj=NULL,
                           prsObj=NULL,
                           stdObj=NULL,
                           objList=NULL,
                           ggtheme=theme_grey(),
                           showPlot=TRUE,
                           pdf=NULL){
    
    if (!is.null(objList)){
        repObj<-objList$repObj;
        prsObj<-objList$prsObj;
        stdObj<-objList$stdObj;
    }
    
    returnRep<-FALSE;
    if (is.null(repObj)||is.character(repObj)){
        repObjfile<-repObj;
        repObj<-readReportFile(repObj);
        if (is.null(repObj)){
            cat("Model report object is NULL. Aborting...\n")
            return(NULL);
        }
        returnRep<-TRUE;
    }    
    if (class(repObj)!="tcsam2015"){
        cat("Input object 'repObj' does not appear to be a TCSAM2015 model repObjults object",
            "Aborting...\n",sep='\n')
        return(NULL);
    }
    
    returnPRS<-FALSE;
    if (is.null(prsObj)||is.character(prsObj)){
        prsObj<-readParamsCSV(prsObj);
        if (!is.null(prsObj)) returnPRS<-TRUE;
    }
    
    returnSTD<-FALSE;
    if (is.null(stdObj)||is.character(stdObj)){
        stdObj<-readStdFile(stdObj);
        if (!is.null(stdObj)) returnSTD<-TRUE;
    }
    
    if (!is.null(pdf)){
        pdf(file=pdf,width=8.5,height=11);
        old.par<-par(omi=c(0.25,0.25,0.25,0.25))
        on.exit(dev.off());
        on.exit(par(old.par),add=TRUE);
    }
    
    plots<-list();
    
    if (!is.null(prsObj)){
        #plot model parameter estimates and std devs (but not priors)
        cat("----------------------------------------------\n")
        cat("plotting model parameters and std devs.\n")
        resLst<-compareModels.ParamEsts(list(tcsam=list(prsObj=prsObj,stdObj=stdObj)),
                                           showPlot=FALSE,pdf=NULL);
        plots$params<-resLst$plots;
        if (showPlot) print(plots$params);
    }
    
    #plot model parameter estimates and priors (but not std's or MCMC posteriors)
    cat("----------------------------------------------\n")
    cat("plotting model parameters and priors.\n")
    plotParameters(repObj);
    
    #plot objective function components
    cat("----------------------------------------------\n")
    cat("plotting objective function values.\n")
    plots$objfun<-compareModels.ObjFunValues(repObj,variable='objfun',showPlot=FALSE);
    if (showPlot) print(plots$objfun);
    
    #plot simulated and observed data
    cat("----------------------------------------------\n")
    cat("plotting simulated and observed data.\n")
    plots$DMCs<-plotDataModelComparisons(repObj,showPlot=FALSE);
    if (showPlot) print(plots$DMCs);
    
    #plot model fits (z-scores, size comps, nlls)
    cat("----------------------------------------------\n")
    cat("plotting model fits.\n")
    plots$zscores<-plotZScoresForAll(repObj,showPlot=FALSE);
    if (showPlot) print(plots$zscores);
    
    #plot population quantities
    cat("----------------------------------------------\n")
    cat("plotting population quantities\n")
    plots$pop.quants<-compareModels.PopQuants(repObj,showPlot=FALSE);
    if (showPlot) print(plots$pop.quants);
    
    #plot selectivities
    cat("----------------------------------------------\n")
    cat("plotting selectivities.\n")
    plots$selfcns<-compareModels.SelFcns.ByPC(repObj,showPlot=FALSE);
    if (showPlot) print(plots$selfcns);
    
    #plot fisheries quantities
    cat("----------------------------------------------\n")
    cat("plotting fisheries results.\n")
    plots$fisheries.results<-compareModels.FisheryResults(repObj,showPlot=FALSE);
    if (showPlot) print(plots$fisheries.results);
    
    #plot surveys quantities
    cat("----------------------------------------------\n")
    cat("plotting surveys results.\n")
    plots$surveys.results<-compareModels.SurveyResults(repObj,showPlot=FALSE);
    if (showPlot) print(plots$surveys.results);

    #TODO: stock-recruit curve(s)
    cat("----------------------------------------------\n")
    cat("TODO: plotting stock-recruit curves.\n")
    
    #TODO: control rule/phase plane plots
    cat("----------------------------------------------\n")
    cat("TODO: control rule/phase plane plots.\n")
    
    ret<-list();
    if (returnRep) ret$repObj<-repObj;
    if (returnPRS) ret$prsObj<-prsObj;
    if (returnSTD) ret$stdObj<-stdObj;
    ret$plots<-plots;
    return(invisible(ret));
}

#plotModelrepObjults(repObj,out.pdf='test.pdf');
