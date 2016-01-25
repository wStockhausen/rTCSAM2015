#'
#'@title Plot z-scores for all components
#'
#'@description Function to plot z-scores for all components.
#'
#'@param repObj - model report list object
#'@param ggtheme - theme for ggplot2
#'@param showPlot - flag to show plots immediately
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return list of lists of ggplot objects
#'
#'@import ggplot2
#'
#'@export
#'
plotZScoresForAll<-function(repObj,
                            ggtheme=theme_grey(),
                            showPlot=TRUE,
                            verbose=FALSE){
    
    #plot z-scores for fits to survey abundance and biomass
    ##plots.srv<-plotZScoresForSurveys(repObj,showPlot=showPlot);
    if (verbose) cat("Plotting z-scores for surveys\n")
    plots.srv<-plotZScoresForFleets(repObj,
                                    type='survey',
                                    showPlot=showPlot,
                                    verbose=verbose);
    
    #plot z-scores for fits to fishery catch abundance and biomass
    ##plots.fsh<-plotZScoresForFisheries(repObj,showPlot=showPlot);
    if (verbose) cat("Plotting z-scores for fisheries\n")
    plots.fsh<-plotZScoresForFleets(repObj,
                                    type='fishery',
                                    showPlot=showPlot,
                                    verbose=verbose);
    
    #plot z-scores for recruit devs
    if (verbose) cat("Plotting z-scores for rec devs\n")
    pDevsLnR<-repObj$mpi$rec$pDevsLnR;
    mdfr<-NULL;
    for (p in names(pDevsLnR)){
        dfr<-reshape2::melt(pDevsLnR[[p]]$finalVals,value.name="zscr");
        dfr$pc<-p;
        mdfr<-rbind(mdfr,dfr);
    }
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    pd<-position_identity();
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
    if (verbose) cat("Plotting z-scores for F-devs\n")
    pDevsLnC<-repObj$mpi$fsh$pDevsLnC;
    mdfr<-NULL;
    for (p in names(pDevsLnC)){
        dfr<-reshape2::melt(pDevsLnC[[p]]$finalVals,value.name="zscr");
        dfr$pc<-p;
        mdfr<-rbind(mdfr,dfr);
    }
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    pd<-position_dodge(0.2)
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
    
    #plot z-scores for selectivity devs
    if (verbose) cat("Plotting z-scores for selectivity devs\n");
    pSs<-list();
    devs<-paste("pDevsS",1:6,sep='');
    for (dev in devs){
        pDevsS<-repObj$mpi$sel[[dev]];
        if (!is.null(pDevsS)){
            mdfr<-NULL;
            for (p in names(pDevsS)){
                dfr<-reshape2::melt(pDevsS[[p]]$finalVals,value.name="zscr");
                dfr$pc<-p;
                mdfr<-rbind(mdfr,dfr);
            }
            ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
            pd<-position_dodge(0.2)
            pS <- ggplot(aes_string(x='Var1',y='zscr',colour='pc',shape='pc',fill='pc'),data=mdfr)
            pS <- pS + geom_point(position=pd,size=3,alpha=0.8)
            pS <- pS + ylim(ylim);
            pS <- pS + xlab('year')
            pS <- pS + ylab('deviation')
            pS <- pS + ggtitle(dev)
            pS <- pS + guides(colour=guide_legend('pc',order=1),
                              fill  =guide_legend('pc',order=1),
                              shape =guide_legend('pc',order=1))
            pS <- pS + ggtheme;
            if (showPlot) print(pS);
            pSs[[dev]]<-pS;
        }
    }
    
    return(invisible(list(surveys=plots.srv,
                          fisheries=plots.fsh,
                          recDevs=pR,
                          fshDevs=pC,
                          selDevs=pSs)));
}