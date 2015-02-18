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
                        sxs=c(mc$SXs,"ALL_SEX"),
                        mss=c(mc$MSs,"ALL_MATURITY"),
                        scs=c(mc$SCs,"ALL_SHELL"),
                        label="",
                        ggtheme=theme_grey(),
                        showPlot=TRUE){
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    zbs<-as.vector(mc$zBs);
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs);
    ISSs<-array(NA,dms,dmnames);#input sample sizes
    ESSs<-array(NA,dms,dmnames);#effective sample sizes
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-fit$sx;
        m<-fit$ms;
        s<-fit$sc;
        y<-yrsp[i];
        ISSs[x,m,s,y]<-fit$fit$ss;
        ESSs[x,m,s,y]<-fit$fit$effN;
    }
    
    idfr<-reshape2::melt(ISSs,value.name='n')
    edfr<-reshape2::melt(ESSs,value.name='n')
    ps<-list();
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
                    p <- p + xlab('sample size');
                    p <- p + ylab('year');
                    p <- p + ggtitle(sbtp);
                    p <- p + guides(color=guide_legend(override.aes=list(alpha=1.0,size=6)));
                    
                    ps[[sbtp]]<-p;
                }
            }
        }
    }
    if (showPlot) plotMulti.GG(plotlist=ps,cols=1);
    return(ps)
}