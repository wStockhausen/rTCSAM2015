#'
#'@title Get fits to catch data (abundance, biomass) as z-scores in a dataframe
#'
#'@description Function to get fits to catch data (abundance, biomass) as z-scores in a dataframe.
#'
#'@param fit - fits list for given catch type
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details None.
#'
#'@export
#'
getMDFR.ZScoresForCatchData<-function(repObjs,
                                      fleet.type='fishery',
                                      verbose=FALSE){
    mdfr<-NULL;
    if (!inherits(repObjs,'tcsam2015.rep')){
        #repObjs should be a list of tcsam2015 model report objects
        for (nm in names(repObjs)){
            mdfrp<-getMDFR.ZScoresForCatchData(repObjs[[nm]],fleet.type=fleet.type,verbose=verbose);
            if (!is.null(mdfrp)) mdfrp$model<-nm;
            mdfr<-rbind(mdfr,mdfrp);
        }
    } else {
        #repObjs is a single tcsam2015 model report object
        if (fleet.type=='fishery'){
            flts<-repObjs$model.fits$fisheries;
            fltNms<-names(flts);
        } else if (fleet.type=='survey'){
            flts<-repObjs$model.fits$surveys;
            fltNms<-names(flts);
        } else {
            ##throw error
        }
        ctNms<-c("index.catch","retained.catch","discard.catch","total.catch");
        for (fltNm in fltNms){
            if (fltNm!=''){
                fleet<-gsub("_"," ",fltNm,fixed=TRUE);
                flt<-flts[[fltNm]];
                for (ctNm in ctNms){
                    catch.type<-gsub("."," ",ctNm,fixed=TRUE);
                    ct<-flt[[ctNm]];
                    if (!is.null(ct)){
                        if (verbose) cat("Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        mdfrp<-NULL;
                        if (!is.null(ct$abundance)){
                            if (verbose) cat("Getting abundance zscores\n")
                            afits<-ct$abundance$fits;
                            mdfrp<-getMDFR.ZScores(afits,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$data.type<-"abundance";
                        }
                        if (!is.null(ct$biomass)){
                            if (verbose) cat("Getting biomass zscores\n")
                            afits<-ct$biomass$fits;
                            mdfrpp<-getMDFR.ZScores(afits,verbose=verbose);
                            if (!is.null(mdfrpp)) {
                                mdfrpp$data.type<-"biomass";
                                mdfrp<-rbind(mdfrp,mdfrpp);
                            }
                        }
                        if (!is.null(mdfrp)){
                            if (verbose) cat("--created dataframe w/",nrow(mdfrp),"rows\n")
                            mdfrp$catch.type<-catch.type;
                            mdfrp$fleet<-fleet;
                            mdfrp$fleet.type<-fleet.type;
                            mdfr<-rbind(mdfr,mdfrp);
                        }
                    }
                }##ctNms
            }
        }##fltNms
        if (!is.null(mdfr)) mdfr$model<-"";
    }        
        
    return(mdfr);
}
