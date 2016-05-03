#'
#'@title Write population processes from model results from TCSAM2015 and rsimTCSAM model runs to a csv file
#'
#'@description Function to write population processes from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param csv - base name for csv files to write to
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
#'
#'@details Extracts natural mortality, mean growth increments, molt-to-maturity, and recruitment 
#'size distribution information.
#'
#'@export
#'
writeModelResultsToCSV.PopProcesses<-function(tcsams=NULL,
                                               rsims=NULL,
                                               csv="ModelResults.PopProcesses",
                                               verbose=FALSE){
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    #natural mortality
    mdfr<-getMDFR.NatMort(tcsams,rsims);
    write.csv(mdfr,file=paste0(csv,".NatMort.csv"),row.names=TRUE);

    #mean growth increments
    mdfr<-getMDFR.MeanGrowthIncrements(tcsams,rsims);
    write.csv(mdfr,file=paste0(csv,".MeanGrowthIncrements.csv"),row.names=TRUE);

    #molt-to-maturity
    write.csv(mdfr,file=paste0(csv,".prM2M.csv"),row.names=TRUE);

    #recruitment size distribution
    mdfr<-getMDFR.RecSizeDistribution(tcsams,rsims,verbose);
    write.csv(mdfr,file=paste0(csv,".RecSizeDist.csv"),row.names=TRUE);

    #recruitment sex ratio
    mdfr<-getMDFR.SexRatio(tcsams,rsims,verbose);
    write.csv(mdfr,file=paste0(csv,".SexRatio.csv"),row.names=TRUE);

    #initial size distribution
    if (verbose) cat("Plotting initial size distribution\n");
    path<-'mr/iN_xmsz';
    csvp<-paste0(csv,".InitialNatZ.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Abundance (millions)",
                                cast.formula="x+m+s+z",csv=csvp);

    if (verbose) cat("Done!\n");
}