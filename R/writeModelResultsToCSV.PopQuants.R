#'
#'@title Write population quantities from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@description Function to write population quantities from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param csv - base name for csv files to write to
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
#'
#'@details none.
#'
#'@export
#'
writeModelResultsToCSV.PopQuants<-function(tcsams=NULL,
                                           rsims=NULL,
                                           csv="ModelResults.PopQuants",
                                           verbose=FALSE){
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (inherits(rsims,'rsimTCSAM')){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    #abundance trends
    if (verbose) cat("Exporting population abundance trends\n");
    path<-'mr/P_list/N_yxmsz';
    csvp<-paste0(csv,".Abundance.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Abundance (millions)",
                                cast.formula="y+x+m",csv=csvp);

    #biomass trends
    if (verbose) cat("Exporting population biomass trends\n");
    path<-'mr/P_list/B_yxms';
    csvp<-paste0(csv,".Biomass.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Biomass (1000's t)",
                                cast.formula="y+x+m",csv=csvp);

    #mature biomass at mating trends
    if (verbose) cat("Exporting population mature biomass-at-mating trends\n");
    path<-'mr/P_list/MB_yx';
    csvp<-paste0(csv,".MMB.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Mature Biomass at Mating",
                                cast.formula="y+x",csv=csvp);

    #recruitment
    if (verbose) cat("Exporting recruitment time series\n");
    path<-'mp/R_list/R_y';
    csvp<-paste0(csv,".Recruitment.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Recruitment (millions)",
                                cast.formula="y",csv=csvp);

    #Population abundance-at-size
    if (verbose) cat("Exporting poulation abundance-at-size\n");
    path<-'mr/P_list/N_yxmsz';
    csvp<-paste0(csv,".N_yxmsz.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Abundance (millions)",
                                cast.formula="y+x+m+s+z",csv=csvp);

    if (verbose) cat("Done!\n");
}