#'
#'@title Plot model-estimated population quantities such as natural mortality rates, maturity schedules, etc.
#'
#'@description Function to plot model-estimated population quantities such as 
#'natural mortality rates, maturity schedules, etc.
#'
#'@param res - model results list from TCSAM 2015 model
#'
#'@export
#'
plotPopQuants<-function(res){
    
    #plot natural mortality estimates by parameter combination
    cat("plotting estimated natural mortality\n")
    plotNM.ByPC(res);
    
    cat("pplotting maturity schedules.\n")
    plotPrMaturity.ByPC(res);
    
    cat("plotting growth transition matrices.\n")
    plotPrGrowth.ByPC(res);
    
    #plot model recruitment time series
    cat("plotting recruits.\n")
    plotRecruits(res,ylab="recruitment (millions)");
    plotRecruits(res,logscale=TRUE,ylab="recruitment (millions) [ln-scale]");
    
    cat("plotting recruitment sex fraction time series.\n")
    plotRecruits.SexRatio(res);
    
    #TODO: plot recruitment size distributions
    cat("TODO: plotting recruitment size distributions.\n")
    
    #plot spawning biomass
    cat("plotting spawning biomass.\n")
    plotSpB(res);
    
    #plot population abundance components
    cat("plotting population abundance components\n")
    plotPopComponents.Abundance(res);
    
    #plot population biomass components
    cat("plotting population biomass components\n")
    plotPopComponents.Biomass(res);
    
}