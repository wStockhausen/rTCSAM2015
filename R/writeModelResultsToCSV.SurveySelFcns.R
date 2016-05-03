#'
#'@title Write survey selectivity functions to csv from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to write survey selectivity functions to csv from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param years - list, by fishery name, of years to plot
#'@param cast.factors - x,m,s factors to include in output file. Non-specified factor levels will be averaged over.
#'@param csv - base name for csv files to write to
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects
#'
#'@details Factors listed in the cast.factors formula will be included as levels in output file.
#'
#'@export
#'
writeModelResultsToCSV.SurveySelFcns<-function(tcsams=NULL,
                                               rsims=NULL,
                                               years=list(`NMFS Trawl Survey`=2014),
                                               cast.factors="x",
                                               csv="ModelResults.SurveySelFcns",
                                               verbose=FALSE){

    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (inherits(rsims,'rsimTCSAM')){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    cast.formula<-paste0("f+z");
    if (!is.null(cast.factors)) cast.formula<-paste0("f+",cast.factors,"+z");
    
    #selectivity/retention
    path<-'mp/S_list/sel_vyxmsz';
    mdfr<-extractModelResults.RepObjs(tcsams,rsims,path,
                                      label.value="sel",
                                      cast.formula=cast.formula,csv=NULL);
    
    dfr<-NULL;
    for (uV in names(years)){
        mdfrp<-mdfr[(tolower(mdfr$v)==tolower(uV))&(mdfr$y %in% years[[uV]]),];#select results for survey uV      
        if (nrow(mdfrp)>0){ dfr<-rbind(dfr,mdfrp);}
    }#uniqVs
    dfr[['.']]<-ifelse(dfr[['.']]==0,NA,dfr[['.']]);
    idx<-which(names(dfr)=='.');
    names(dfr)[idx]<-'sel';
    
    write.csv(dfr,file=paste0(csv,".csv"),row.names=FALSE)
    
    if (verbose) cat("Done.\n")
}
