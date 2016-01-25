#'
#'@title Compare surveys-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare surveys-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModels.SurveyResults<-function(tcsam=NULL,
                                      rsim=NULL,
                                      showPlot=TRUE,
                                      pdf=NULL,
                                      width=8,
                                      height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    plots<-list();
    
    #survey Qs
    p<-compareModels.SurveyQs(tcsam,rsim,showPlot=showPlot)
    plots$surveyQs<-p;
    
    #survey abundances
    p<-compareModels.SurveyAbundance(tcsam,rsim,showPlot=showPlot)
    plots$surveyAbundance<-p;
    
    #survey biomass
    p<-compareModels.SurveyBiomass(tcsam,rsim,showPlot=showPlot)
    plots$surveyBiomass<-p;
    
    return(invisible(plots))
}