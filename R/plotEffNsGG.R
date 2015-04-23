#'
#'@title Plot input and estimated sample sizes from size frequency fits
#'
#' @description  Plot input and estimated sample sizes
#' from size frequency fits using ggplot2.
#' 
#' @param fits - list of fits
#' @param mc - model configuration list
#' @param sxs - character vector of sexes to plot
#' @param mss - character vector of maturity states to plot
#' @param scs - character vector of shell conditions to plot
#' @param label - plot label
#' @param ggtheme - ggplot2 theme
#' @param showPlot - flag to show (print) plot immediately on current graphics device
#' 
#' @return list of ggplot2 plot objects
#' 
#' @import reshape2
#' @import ggplot2
#' 
#' @export
#' 
plotEffNsGG<-function(fits,
                        mc,
                        sxs=c(mc$dims$x$nms,"ALL SEX"),
                        mss=c(mc$dims$m$nms,"ALL MATURITY"),
                        scs=c(mc$dims$s$nms,"ALL SHELL"),
                        label="",
                        ggtheme=theme_grey(),
                        showPlot=TRUE){
    
    sxs<-tolower(sxs); #use lower case for all indices
    mss<-tolower(mss);
    scs<-tolower(scs);
    
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs);
    ISSs<-array(NA,dms,dmnames);#input sample sizes
    ESSs<-array(NA,dms,dmnames);#effective sample sizes
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        ISSs[x,m,s,y]<-fit$fit$ss;
        ESSs[x,m,s,y]<-fit$fit$effN;
    }
    
    idfr<-reshape2::melt(ISSs,value.name='n')
    edfr<-reshape2::melt(ESSs,value.name='n')
    plots<-list();
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                if (sum(ISSs[x,m,s,],na.rm=TRUE)>0){
                    #set up labels
                    sbt<-vector(mode="character",length=3);
                    if (substr(x,1,3)!="ALL") {sbt[1]<-x;}
                    if (substr(m,1,3)!="ALL") {sbt[2]<-m;}
                    if (substr(s,1,3)!="ALL") {sbt[3]<-s;}
                    sbtp<-tolower(paste(sbt[sbt!=""],collapse=", "));
                    if (label!='') sbtp<-paste(label,sbtp,sep=': ');
                    
                    #extract ISSs and ESSs
                    idx<-(idfr$sx %in% x)&(idfr$ms %in% m)&(idfr$sc %in% s)
                    idfrp<-idfr[idx,4:5];
                    idfrp$type<-'input';
                    idx<-(edfr$sx %in% x)&(edfr$ms %in% m)&(edfr$sc %in% s)
                    edfrp<-edfr[idx,4:5];
                    edfrp$type<-'effective';
                    
                    dfrp<-rbind(idfrp,edfrp);
                    p <- ggplot(aes_string(x='yr',y='n',colour='type'),data=dfrp);
                    p <- p + geom_point();
                    p <- p + geom_line();
                    p <- p + ylab('sample size');
                    p <- p + xlab('year');
                    p <- p + ggtitle(sbtp);
                    p <- p + guides(color=guide_legend(override.aes=list(alpha=1.0,size=6)));
                    
                    plots[[sbtp]]<-p;
                }
            }
        }
    }
    if (showPlot) plotMulti.GG(plotlist=plots,cols=1);
    return(invisible(plots));
}