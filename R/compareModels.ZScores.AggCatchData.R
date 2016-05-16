#'
#'@title Compare zscores for fits to aggregated catch data (abundance or biomass) for TCSAM2015 model runs
#'
#'@description Function to compare zscores for fits to aggregated catch data (abundance or biomass) for TCSAM2015 model runs.
#'
#'@param mdfr - dataframe from call to \code{getMDFR.ZScoresForFleets(...)}
#'@param fleets - fleets to plot [NULL for all]
#'@param catch.types - catch types ('index.catch', retained.catch', etc) to plot [NULL for all]
#'@param logscale - flag (T/F) to plot on log scale
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param ggtheme - graphical theme for plot
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects, named by fleet
#'
#'@details Plots are produced by fleet and faceted by sex, maturity, shell condition, and catch type.
#'
#'@import ggplot2
#'
#'@export
#'
compareModels.ZScores.AggCatchData<-function(mdfr,
                                              fleets=NULL,
                                              catch.types=NULL,
                                              xlab="Year",
                                              ylab="",
                                              xlim=NULL,
                                              ylim=NULL,
                                              ggtheme=ggplot2::theme_grey(),
                                              showPlot=TRUE,
                                              pdf=NULL,
                                              width=8,
                                              height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    if (is.null(fleets)) fleets<-sort(unique(mdfr$fleet));#fleet names
    if (is.null(catch.types)) catch.types<-c("index catch",
                                             "total catch",
                                             "retained catch",
                                             "discard catch");
    
    mdfr1<-mdfr[(mdfr$fleet %in% fleets)&(mdfr$catch.type %in% catch.types),];
    
    plots<-list();
    for (fleet in fleets){
        if (verbose) cat("Plotting fleet '",fleet,"'.\n",sep='');
        mdfrp<-mdfr1[mdfr1$fleet==fleet,];#select results for fleet        

        #do plot
        pd<-position_identity();
        ylim<-max(abs(mdfrp$val),na.rm=TRUE)*c(-1,1);
        p3 <- ggplot(aes_string(x='y',y='val',colour='model',shape='model',fill='model'),data=mdfrp)
        p3 <- p3 + geom_point(position=pd,size=3,alpha=0.8)
        p3 <- p3 + geom_hline(yintercept=0,colour='black',size=2,linetype=2)
        p3 <- p3 + xlim(xlim);
        p3 <- p3 + ylim(ylim);
        p3 <- p3 + ylab('z-scores')
        p3 <- p3 + facet_grid(x+m+s+catch.type~.)
        p3 <- p3 + guides(colour=guide_legend('model',order=1),
                          fill  =guide_legend('model',order=1),
                          shape =guide_legend('model',order=1))
        p3 <- p3 + ggtitle(fleet);
        p3 <- p3 + ggtheme;
        print(p3)
        plots[[fleet]]<-p3;
    }#fleets
    
    return(invisible(plots))
}
