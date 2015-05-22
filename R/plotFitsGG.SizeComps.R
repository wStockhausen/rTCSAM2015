#'
#'@title Plot fits to size comps
#'
#' @description  Plot size comps fits using ggplot2.
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
plotFitsGG.SizeComps<-function(fits,
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
    oAtZ<-array(0,dms,dmnames);#observed size comps
    mAtZ<-array(0,dms,dmnames);#model size comps
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs);
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        oAtZ[x,m,s,y,]<-fit$fit$obs;
        mAtZ[x,m,s,y,]<-fit$fit$mod;
    }
    
    odfr<-reshape2::melt(oAtZ,value.name='comp');
    mdfr<-reshape2::melt(mAtZ,value.name='comp');
    odfr$type<-'observed';
    mdfr$type<-'estimated';
    pdfr<-rbind(odfr,mdfr);
    ps<-list();
    odx<-(pdfr$type=='observed');
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                #set up extraction indices
                idx<-(pdfr$sx %in% x)&(pdfr$ms %in% m)&(pdfr$sc %in% s);
                if (sum(pdfr$comp[idx&odx],na.rm=TRUE)>0){
                    #set up labels
                    sbt<-vector(mode="character",length=3);
                    if (substr(x,1,3)!="all") {sbt[1]<-x;}
                    if (substr(m,1,3)!="all") {sbt[2]<-m;}
                    if (substr(s,1,3)!="all") {sbt[3]<-s;}
                    sbtp<-tolower(paste(sbt[sbt!=""],collapse=", "));
                    if (label!='') sbtp<-paste(label,sbtp,sep=': ')
                    
                    #check normalization
                    tst<-dcast(pdfr[idx,],sx+ms+sc+yr~type,fun.aggregate=sum,value.var='comp')
                    cat("Normalization check:\n");
                    print(tst)
                    
                    pl <- ggplot(aes(x=zb,y=comp,color=type,fill=type),data=pdfr[idx&odx,]);
                    pl <- pl + geom_bar(alpha=0.8,stat='identity');
                    pl <- pl + geom_line(data=pdfr[idx&(!odx),],alpha=0.8);
                    pl <- pl + labs(x='size (mm)',y='composition');
                    pl <- pl + facet_wrap(~yr,nr=10);
                    pl <- pl + ggtitle(sbtp);
                    pl <- pl + guides(fill=guide_legend('type'),colour=guide_legend('type'))
                    if (showPlot) print(pl);
                    ps[[sbtp]]<-pl;
                }#sum(pdfr$comp[idx&odx],na.rm=TRUE)>0
            }#s
        }#m
    }#x
    cat("---Done running plotFitsGG.SizeComps(...)\n\n");
    return(ps)
}