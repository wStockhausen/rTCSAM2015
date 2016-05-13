#'
#'@title Get model fits to abundance, biomass or size frequencies for fleet data components
#'
#'@title Function to get model fits to abundance, biomass or size frequencies for fleet data components.
#'
#'@param repObjs - single model report list object, or named list of them
#'@param fleet.type - fleet type ('fishery' or 'survey')
#'@param data.type - data type ('abundance', 'biomass', or 'n.at.z')
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return dataframe
#'
#'@details Uses \code{getMDFR.FitsForABData()} and \code{getMDFR.FitsForSizeComps()}.
#' Returned dataframe has columns:
#' \itemize{
#'  \item{x - sex}
#'  \item{m - maturity}
#'  \item{s - shell condition}
#'  \item{y - year}
#'  \item{z - size bin [if data.type was 'n.at.z']}
#'  \item{val - value}
#'  \item{var - variable type}
#'  \item{data.type - data type ('abundance','biomass', or 'n.at.z')}
#'  \item{catch.type - catch type ('index.catch','retained.catch', etc.)}
#'  \item{fleet - fleet name}
#'  \item{fleet.type - fleet type ('survey' or 'fishery')}
#'  \item{model - model name}
#' }
#' Note: the 'z' column will be present only if the data.type requested was 'n.at.z'.
#'
#'@export
#'
getMDFR.FitsForFleets<-function(repObjs,
                                fleet.type='fishery',
                                data.type='abundance',
                                verbose=FALSE){
    if (verbose) cat("--Starting getMDFR.FitsForFleets.\n");
    mdfr<-NULL;
    if (!inherits(repObjs,'tcsam2015.rep')){
        #repObjs should be a list of tcsam2015 model report objects
        for (nm in names(repObjs)){
            mdfrp<-getMDFR.FitsForFleets(repObjs[[nm]],fleet.type=fleet.type,
                                            data.type=data.type,verbose=verbose);
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
                        if (verbose) cat("---Getting '",ctNm,"' for ",fltNm,"\n",sep='');
                        mdfrp<-NULL;
                        if ((data.type=="abundance")&&!is.null(ct$abundance)){
                            if (verbose) cat("---Getting abundance zscores\n")
                            mdfrp<-getMDFR.FitsForABData(ct$abundance$fits,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$data.type<-"abundance";
                        } else
                        if ((data.type=="biomass")&&!is.null(ct$biomass)){
                            if (verbose) cat("---Getting biomass zscores\n")
                            mdfrp<-getMDFR.FitsForABData(ct$biomass$fits,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$data.type<-"biomass";
                        } else
                        if ((data.type=="n.at.z")&&!is.null(ct$n.at.z)){
                            if (verbose) cat("---Getting n.at.z zscores\n")
                            mdfrp<-getMDFR.FitsForSizeComps(ct$n.at.z,repObjs$mc,verbose=verbose);
                            if (!is.null(mdfrp)) mdfrp$data.type<-"n.at.z";
                        }
                        if (!is.null(mdfrp)){
                            if (verbose) cat("---created dataframe w/",nrow(mdfrp),"rows\n")
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
        
    if (verbose) cat("--Finished getMDFR.FitsForFleets.\n");
    return(mdfr);
}
