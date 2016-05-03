#'
#'@title Write fishery selectivity functions to csv from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to write fishery selectivity functions to csv from TCSAM2015 and rsimTCSAM model runs.
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
writeModelResultsToCSV.FisherySelFcns<-function(tcsams=NULL,
                                                rsims=NULL,
                                                years=list(TCF=2013),
                                                cast.factors="x",
                                                csv="ModelResults.FisheriesSelFcns",
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
    path<-'mp/F_list/sel_fyxmsz';
    sdfr<-extractModelResults.RepObjs(tcsams,rsims,path,
                                      label.value="sel/ret",
                                      cast.formula=cast.formula,csv=NULL);
    sdfr$type<-'selectivity';
    path<-'mp/F_list/ret_fyxmsz';
    rdfr<-extractModelResults.RepObjs(tcsams,rsims,path,
                                      label.value="sel/ret",
                                      cast.formula=cast.formula,csv=NULL);
    rdfr$type<-'retention';

    dfr<-NULL;
    for (uF in names(years)){
        mdfrp<-mdfr[(tolower(mdfr$f)==tolower(uF))&(mdfr$y %in% years[[uF]]),];#select results for fishery F      
        if (nrow(mdfrp)>0){ dfr<-rbind(dfr,mdfrp);}
    }#uniqFs
    dfr[['.']]<-ifelse(dfr[['.']]==0,NA,dfr[['.']]);
    idx<-which(names(dfr)=='.');
    names(dfr)[idx]<-'sel/ret';
    
    write.csv(dfr,file=paste0(csv,".csv"),row.names=FALSE)
    
    if (verbose) cat("Done.\n")
}
