#'
#'@title Plot model fits to abundance, biomass and size frequencies as z-scores for fleet data components
#'
#'@title Function to plot model fits to abundance, biomass and size frequencies as z-scores for fleet data components.
#'
#'@param repObj - model report list object
#'@param type - fleet type ('fishery' or 'survey')
#'@param showPlot - flag (T/F) to show plots immediately
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return list by fleet of lists with ggplot objects
#'
#'@details Uses \code{plotZScoresForCatchData()}.
#'
#'@export
#'
plotZScoresForFleets<-function(repObj,
                               type='fishery',
                               showPlot=TRUE,
                               verbose=FALSE){
    plots<-list();
    if (type=='fishery'){
        flts<-repObj$model.fits$fisheries;
        fltNms<-names(flts);
    } else if (type=='survey'){
        flts<-repObj$model.fits$surveys;
        fltNms<-names(flts);
    } else {
        ##throw error
    }
    
    ctNms<-c("index.catch","retained.catch","discard.catch","total.catch");
    for (fltNm in fltNms){
        if (fltNm!=''){
            flt<-flts[[fltNm]];
            plots.ct<-list();
            for (ctNm in ctNms){
                ct<-flt[[ctNm]];
                if (!is.null(ct)){
                    if (verbose) cat("Plotting '",ctNm,"' for ",fltNm,"\n",sep='');
                    plts<-plotZScoresForCatchData(ct,
                                                  repObj$mc,
                                                  fleet=gsub("_"," ",fltNm,fixed=TRUE),
                                                  type=paste(type,gsub("."," ",ctNm,fixed=TRUE)),
                                                  showPlot=showPlot,
                                                  verbose=verbose);
                    plots.ct[[ctNm]]<-plts;
                }
            }##ctNms
            plots[[fltNm]]<-plots.ct;
        }
    }##fltNms
    return(plots);
}
