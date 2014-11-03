#'
#'@title Plot residuals from size frequency fits
#'
#' @description  Plot Pearson's residuals or negative log-likelihood components
#' from size frequency fits.
#' 
#' @param fits - list of fits
#' @param mc - model configuration list
#' @param plotPearsons - flag (T/F) to plot Pearson's residuals (T) or NLLs (F)
#' @param label - plot label
#' 
#' @export
#' 
plotZScoresForSizeFreqs<-function(fits,
                                  mc,
                                  plotPearsons=TRUE,
                                  sxs=c(mc$SXs,"ALL_SEX"),
                                  mss=c(mc$MSs,"ALL_MATURITY"),
                                  scs=c(mc$SCs,"ALL_SHELL_CONDITION"),
                                  label=""){
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    zbs<-as.vector(mc$zBs);
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs,zb=zbs);
    rAtZ<-array(0,dms,dmnames);
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-fit$sx;
        m<-fit$ms;
        s<-fit$sc;
        y<-yrsp[i];
        if (plotPearsons){
            pt<-"Pearson's residuals";
            rAtZ[x,m,s,y,]<-fit$fit$zscrs;
        } else {
            rAtZ[x,m,s,y,]<-fit$fit$nlls;
            pt<-"nll's";
        }
    }
    
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                rAtZp<-t(as.matrix(rAtZ[x,m,s,,]));
                if (sum(abs(rAtZp),na.rm=TRUE)>0){
                    sbt<-vector(mode="character",length=3);
                    if (substr(x,1,3)!="ANY") {sbt[1]<-x;}
                    if (substr(m,1,3)!="ANY") {sbt[2]<-m;}
                    if (substr(s,1,3)!="ANY") {sbt[3]<-s;}
                    sbtp<-paste(sbt[sbt!=""],collapse=", ");
                    plotCompsAsCircles(rAtZp,yrs,zbs,maxRadius=0.6,
                                       main=label,subtitle=sbtp,
                                       transparency=0.6,xlab="year",ylab="size (mm CW)");
                    mtext(pt,side=1,adj=0.01,outer=TRUE,line=-1);
                }
            }
        }
    }
}