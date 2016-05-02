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
    #total catch abundance
    path<-'mr/F_list/cpN_fyxmsz';
    csvp<-paste0(csv,".TotalCatchAbundance.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Total catch (millions)",
                                cast.formula=cast.formula,csv=csvp);
    
    #total retined catch
    path<-'mr/F_list/rmN_fyxmsz';
    csvp<-paste0(csv,".RetainedCatchAbundance.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Retained catch (millions)",
                                cast.formula=cast.formula,csv=csvp);
    
    #total catch biomass
    path<-'mr/F_list/cpB_fyxms';
    csvp<-paste0(csv,".TotalCatchBiomass.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Total catch (1000's t)",
                                cast.formula=cast.formula,csv=csvp);
    
    #retained mortality (biomass)
    path<-'mr/F_list/rmB_fyxms';
    csvp<-paste0(csv,".RetainedCatchBiomass.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Retained catch (1000's t)",
                                cast.formula=cast.formula,csv=csvp);
    
    #discard mortality (biomass)
    path<-'mr/F_list/dmB_fyxms';
    csvp<-paste0(csv,".DiscardMortalityBiomass.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Discard mortality (1000's t)",
                                cast.formula=cast.formula,csv=csvp);
    
    if (verbose) cat("Done!\n");
}
