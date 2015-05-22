#'
#'@title Plot residuals from size frequency fits
#'
#' @description  Plot Pearson's residuals or negative log-likelihood components
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
#' @return list of list of ggplot2 plot objects
#' 
#' @import reshape2
#' 
#' @export
#' 
plotZScoresGG.SizeFreqs<-function(fits,
                                    mc,
                                    sxs=c(mc$dims$x$nms,"ALL_SEX"),
                                    mss=c(mc$dims$m$nms,"ALL_MATURITY"),
                                    scs=c(mc$dims$s$nms,"ALL_SHELL"),
                                    label="",
                                    ggtheme=theme_grey(),
                                    showPlot=TRUE){
    cat("---Running plotFitsGG.SizeComps(...) for",label,"\n");
    
    label<-gsub("[_]"," ",label);#replace "_"'s with blank spaces
    
    dims<-mc$dims;
    sxs<-gsub("_"," ",tolower(sxs),fixed=TRUE);
    mss<-gsub("_"," ",tolower(mss),fixed=TRUE);
    scs<-gsub("_"," ",tolower(scs),fixed=TRUE);
    
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    zbs<-dims$z$vls;
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs,zb=zbs);
    pAtZ<-array(0,dms,dmnames);#pearson's residuals
    nAtZ<-array(0,dms,dmnames);#nll's residuals
    
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
        pAtZ[x,m,s,y,]<-fit$fit$zscrs;
        nAtZ[x,m,s,y,]<-fit$fit$nlls;
        ISSs[x,m,s,y]<-fit$fit$ss;
        ESSs[x,m,s,y]<-fit$fit$effN;
    }
    
    pdfr<-reshape2::melt(pAtZ,value.name='residual');
    ndfr<-reshape2::melt(nAtZ,value.name='residual');
    idfr<-reshape2::melt(ISSs,value.name='n')
    edfr<-reshape2::melt(ESSs,value.name='n')
    ps<-list();
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                pAtZp<-t(as.matrix(pAtZ[x,m,s,,]));
                if (sum(abs(pAtZp),na.rm=TRUE)>0){
                    #set up labels
                    sbt<-vector(mode="character",length=3);
                    if (substr(x,1,3)!="all") {sbt[1]<-x;}
                    if (substr(m,1,3)!="all") {sbt[2]<-m;}
                    if (substr(s,1,3)!="all") {sbt[3]<-s;}
                    sbtp<-tolower(paste(sbt[sbt!=""],collapse=", "));
                    if (label!='') sbtp<-paste(label,sbtp,sep=': ')
                    
                    #extract pearsons residuals and plot
                    idx<-(pdfr$sx %in% x)&(pdfr$ms %in% m)&(pdfr$sc %in% s)
                    pdfrp<-pdfr[idx,4:6];
                    pdfrp$type<-'O > M';
                    pdfrp$type[pdfrp$residual<0]<-'O < M';
                    pdfrp$residual<-abs(pdfrp$residual);
                    pr<-plotCompsAsCirclesGG(pdfrp,
                                            x='yr',y='zb',z='residual',
                                            category='type',
                                            title=sbtp,
                                            xlab="pearson's residuals",
                                            ylab="size (mm CW)",
                                            alpha=0.6,
                                            ggtheme=ggtheme,
                                            showPlot=FALSE);
                    
                    #extract nlls and plot
                    idx<-(ndfr$sx %in% x)&(ndfr$ms %in% m)&(ndfr$sc %in% s)
                    ndfrp<-ndfr[idx,4:6];
                    ndfrp$type<-'O > M';
                    ndfrp$type[ndfrp$residual<0]<-'O < M';
                    ndfrp$residual<-abs(ndfrp$residual);
                    pn<-plotCompsAsCirclesGG(ndfrp,
                                            x='yr',y='zb',z='residual',
                                            category='type',
                                            title='',
                                            xlab="negative loglikeihoods",
                                            ylab="size (mm CW)",
                                            alpha=0.6,
                                            ggtheme=ggtheme,
                                            showPlot=FALSE);
                    
                    if (showPlot) plotMulti.GG(pr,pn,cols=1);
                    
                    ps[[sbtp]]<-list(pearsons=pr,nlls=pn);
                }
            }
        }
    }
    cat("---Done running plotFitsGG.SizeComps(...)\n\n");
    return(ps)
}