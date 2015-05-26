#'
#'@title Plot model fits to data components
#'
#'@param repObj - model report list object
#'@param ggtheme - theme for ggplot2
#'@param showPlot - flag to show plots immediately
#'
#'@return list of lists of ggplot objects
#'
#'@export
#'
plotZScoresForAll<-function(repObj,
                            ggtheme=theme_grey(),
                            showPlot=FALSE){
    
    #plot z-scores for fits to survey abundance and biomass
    plots.srv<-plotZScoresForSurveys(repObj,showPlot=showPlot);
    
    #plot z-scores for fits to fishery catch abundance and biomass
    plots.fsh<-plotZScoresForFisheries(repObj,showPlot=showPlot);
    
    #plot z-scores for recruit devs
    cat("Plotting z-scoress for rec devs\n")
    pDevsLnR<-repObj$mpi$rec$pDevsLnR;
    mdfr<-NULL;
    for (p in names(pDevsLnR)){
        dfr<-reshape2::melt(pDevsLnR[[p]]$finalVals,value.name="zscr");
        dfr$pc<-p;
        mdfr<-rbind(mdfr,dfr);
    }
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    pd<-position_identity(0.0)
    pR <- ggplot(aes_string(x='Var1',y='zscr',colour='pc',shape='pc',fill='pc'),data=mdfr)
    pR <- pR + geom_point(position=pd,size=3,alpha=0.8)
    pR <- pR + ylim(ylim);
    pR <- pR + xlab('year')
    pR <- pR + ylab('recruitment deviations')
    pR <- pR + guides(colour=guide_legend('pc',order=1),
                      fill  =guide_legend('pc',order=1),
                      shape =guide_legend('pc',order=1))
    pR <- pR + ggtheme;
    if (showPlot) print(pR);
    
    #plot z-scores for F-devs
    cat("Plotting z-scoress for F-devs\n")
    pDevsLnC<-repObj$mpi$fsh$pDevsLnC;
    mdfr<-NULL;
    for (p in names(pDevsLnC)){
        dfr<-reshape2::melt(pDevsLnC[[p]]$finalVals,value.name="zscr");
        dfr$pc<-p;
        mdfr<-rbind(mdfr,dfr);
    }
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    pd<-position_identity(0.2)
    pC <- ggplot(aes_string(x='Var1',y='zscr',colour='pc',shape='pc',fill='pc'),data=mdfr)
    pC <- pC + geom_point(position=pd,size=3,alpha=0.8)
    pC <- pC + ylim(ylim);
    pC <- pC + xlab('year')
    pC <- pC + ylab('fishery capture deviations')
    pC <- pC + guides(colour=guide_legend('pc',order=1),
                      fill  =guide_legend('pc',order=1),
                      shape =guide_legend('pc',order=1))
    pC <- pC + ggtheme;
    if (showPlot) print(pC);
    
    return(invisible(list(surveys=plots.srv,fisheries=plots.fsh,recDevs=pR,fshDevs=pC)))
}