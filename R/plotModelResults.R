#'
#'@title Plot TCSAM2015 model output.
#'
#'@description Function to plot data and results from a TCSAM2015 model run.
#'
#'@param repObj - list object based on sourcing a TCSAM2015 model report file. can be NULL or a filename.
#'@param prsObj - list object based on reading a TCSAM2015 active parameters csv file. can be NULL or a filename.
#'@param stdObj - list object based on reading a TCSAM2015 std file. can be NULL or a filename.
#'@param objList - list with optional elements repObj, prsObj, stdObj (an optional way to provide the Obj's)
#'@param ggtheme - a ggplot2 theme to use with ggplot2 plots
#'@param showPlot - flag to show plots immediately
#'@param pdf - filename for pdf output (optional)
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return list with possible elements:\cr
#'* repObj - TCSAM2015 report list object (if report file was read)
#'* prsObj - TCSAM2015 parameters dataframe object (if parameters csv file was read)
#'* stdObj - TCSAM2015 std devs dataframe object (if std file was read)
#'* plots: multi-level list of ggplot2 objects
#'
#'@details If repObj is a character string (the path to a report file), the file will be read in.
#'If repObj is NULL, the user will be prompted to identify a 
#'model report file from which to source the report object. Similarly for prsObj and stdObj.
#'Any of these objects obtained by reading a file will be an element in the returned list.
#'
#'@export
#'
plotModelResults<-function(repObj=NULL,
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
        cat("Input object 'repObj' does not appear to be a TCSAM2015 model report object",
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
    
    plots<-plotModelResultsI(repObj=repObj,
                             prsObj=prsObj,
                             stdObj=stdObj,
                             ggtheme=ggtheme,
                             showPlot=showPlot,
                             pdf=pdf,
                             width=width,
                             height=height,
                             verbose=verbose);

    ret<-list();
    if (returnRep) ret$repObj<-repObj;
    if (returnPRS) ret$prsObj<-prsObj;
    if (returnSTD) ret$stdObj<-stdObj;
    ret$plots<-plots;
    return(invisible(ret));
}

#plotModelResults(repObj,pdf='test.pdf');
