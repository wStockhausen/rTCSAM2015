#'
#'@title Plot time series of the fully-selected fishery capture rates.
#'
#'@description Plot time series of the fully-selected fishery capture rates.
#'
#'@param res - results list from a TCSAM2015 model run
#'@param ggtheme - a ggplot2 theme
#'@param showPlot - flag (T/F) to show plots
#'
#'@return list of ggplot objects
#'
#'@import ggplot2
#'
#'@export
#'
plotFishingRatesGG<-function(res,
                             ggtheme=theme_bw(),
                             showPlot=FALSE){
    dfr<-NULL;
    nFsh<-res$mc$nFsh;
    for (f in 1:nFsh){
        cat('f =',f,'\n')
        fnm<-res$mc$lbls.fsh[f];
        cp<-reshape2::melt(res$fisheries[[f]]$cap$F.xmsyz,value.name='F')
        cp$fishery<-fnm;
        cp$type<-'capture';
        dm<-reshape2::melt(res$fisheries[[f]]$dm$F.xmsyz,value.name='F')
        dm$fishery<-fnm;
        dm$type<-'discards mortality';
        dfr<-rbind(dfr,cp,dm);
        if (!is.null(res$fisheries[[f]]$rm)){
            rm<-reshape2::melt(res$fisheries[[f]]$rm$F.xmsyz,value.name='F')
            rm$fishery<-fnm;
            rm$type<-'retained mortality';
            dfr<-rbind(dfr,rm);
        }
    }
    dfr$year<-as.numeric(dfr$year);
    
    #select max F by type/fishery/year/sex/maturity/shell condition
    dfr<-reshape2::dcast(dfr,type+fishery+year+sex+maturity+shell_condition~.,max,
                         value.var='F',drop=TRUE)
    nms<-names(dfr);
    nms[7]<-'maxF';
    names(dfr)<-nms;
    dfr$xms<-paste(dfr$sex,dfr$maturity,dfr$shell_condition,sep=', ')
    
    
    rng<-c(0,max(dfr$maxF,na.rm=TRUE));
    
    uxms<-unique(dfr$xms);
    
    ps<-list();
    for (xms in uxms){    
        dfrp<-dfr[dfr$xms==xms,];
        p <- ggplot(data=dfrp)
        p <- p + geom_line(aes(x=year,y=maxF,colour=type),size=1)
        p <- p + scale_x_continuous(breaks=pretty(dfrp$year)) 
        p <- p + scale_y_continuous(breaks=pretty(dfrp$maxF),limits=rng,expand=c(0.01,0))
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
        p <- p + labs(x="year",y=expression("Max. Fishing Rate ("*yr^-1*")"))
        p <- p + ggtitle(tolower(xms))
        p <- p + guides(colour=guide_legend(''))
        p <- p + facet_wrap(~fishery,ncol=2) 
        p <- p + ggtheme
        if (showPlot) print(p);
        ps[[tolower(xms)]]<-p;
    }
    return(ps)
}

#ps<-plotFishingRatesGG(res);