#'
#'@title Write fishery quantities from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@description Function to write fishery quantities from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param cast.formula - formula for casting (aggregating)
#'@param csv - base name for csv files to write to
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
#'
#'@details none.
#'
#'@export
#'
writeModelResultsToCSV.Fisheries<-function(tcsams=NULL,
                                           rsims=NULL,
                                           cast.formula="f+y+x",
                                           csv="ModelResults.Fisheries",
                                           verbose=FALSE){
    #abundance
    path<-'mr/S_list/N_fyxmsz';
    csvp<-paste0(csv,".Abundance.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Catch (millions)",
                                cast.formula=cast.formula,csv=csvp);
    #biomass
    path<-'mr/S_list/B_fyxms';
    csvp<-paste0(csv,".Biomass.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Catch (1000's t)",
                                cast.formula=cast.formula,csv=csvp);
}
