#'
#'@title Plot time series of the fishery yield (biomass: captured, discards, retained) by fishery and sex.
#'
#'@description Plot time series of the fishery yield (biomass: captured, discards, retained) by fishery and sex.
#'
#'@param res - results list from a TCSAM2015 model run
#'@param showPlot - flag (T/F) to show plots
#'
#'@return list of ggplot objects
#'
#'@import ggplot2 
#'@importFrom reshape2 melt
#'
#'@export
#'
plotFisheryYieldsGG<-function(res,
                              showPlot=TRUE){
    dfr<-NULL;
    nFsh<-res$mc$nFsh;
    for (f in 1:nFsh){
        cat('f =',f,'\n')
        fnm<-res$mc$lbls.fsh[f];
        cpN<-reshape2::melt(res$fisheries[[f]]$cap$b.xy,value.name='yield')
        cpN$fishery<-fnm;
        cpN$type<-'capture';
        dmN<-reshape2::melt(res$fisheries[[f]]$dm$b.xy,value.name='yield')
        dmN$fishery<-fnm;
        dmN$type<-'discards mortality';
        dfr<-rbind(dfr,cpN,dmN);
        if (!is.null(res$fisheries[[f]]$rm)){
            rmN<-reshape2::melt(res$fisheries[[f]]$rm$b.xy,value.name='yield')
            rmN$fishery<-fnm;
            rmN$type<-'retained mortality';
            dfr<-rbind(dfr,rmN);
        }
    }
    dfr$year<-as.numeric(dfr$year);
    
    ps<-list();
    
    #male catch
    dfrp<-dfr[tolower(dfr$sex)=='male',]
    p <- ggplot()
    p <- p + geom_bar(aes(x=year,y=yield,fill=type),data=dfrp[dfrp$type=='capture',],stat="identity",position='identity',alpha=1.0)
    p <- p + geom_bar(aes(x=year,y=yield,fill=type),data=dfrp[dfrp$type!='capture',],stat="identity",position='stack',alpha=1.0)
    p <- p + scale_x_continuous(breaks=pretty(dfrp$year)) 
    p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
    p <- p + labs(x="year",y="Fishery Yield (1,000's t)")
    p <- p + ggtitle('males')
    p <- p + guides(color=guide_legend(''))
    p <- p + facet_wrap(~fishery,ncol=2) 
    if (showPlot) print(p)
    ps$male<-p;
    
    #female catch
    dfrp<-dfr[tolower(dfr$sex)=='female',]
    p <- ggplot()
    p <- p + geom_bar(aes(x=year,y=yield,fill=type),data=dfrp[dfrp$type=='capture',],stat="identity",position='identity',alpha=1.0)
    p <- p + geom_bar(aes(x=year,y=yield,fill=type),data=dfrp[dfrp$type!='capture',],stat="identity",position='stack',alpha=1.0)
    p <- p + scale_x_continuous(breaks=pretty(dfrp$year)) 
    p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
    p <- p + labs(x="year",y="Fishery Yield (1,000's t)")
    p <- p + ggtitle('females')
    p <- p + guides(color=guide_legend(''))
    p <- p + facet_wrap(~fishery,ncol=2) 
    if (showPlot) print(p)
    ps$female<-p;
    
    return(ps)
}

#ps<-plotFisheryYieldsGG(res)