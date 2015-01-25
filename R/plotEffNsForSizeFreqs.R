#'
#'@title Plot ISSs and ESSs from size frequency fits as time series
#'
#' @description  Function to plot input and effective sample sizes (ISSs and ESSs) from size frequencies as time series.
#' 
#' @param fits - list of fits
#' @param mc - model configuration list
#' @param label - plot label
#' 
#' @export
#' 
plotEffNsForSizeFreqs<-function(fits,
                                mc,
                                sxs=c(mc$SXs,"ALL_SEX"),
                                mss=c(mc$MSs,"ALL_MATURITY"),
                                scs=c(mc$SCs,"ALL_SHELL"),
                                label=""){
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
    
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                isss<-as.vector(ISSs[x,m,s,]);
                esss<-as.vector(ESSs[x,m,s,]);
                print(isss);
                print(esss);
                if (sum(abs(isss),na.rm=TRUE)>0){
                    sbt<-vector(mode="character",length=3);
                    if (substr(x,1,3)!="ANY") {sbt[1]<-x;}
                    if (substr(m,1,3)!="ANY") {sbt[2]<-m;}
                    if (substr(s,1,3)!="ANY") {sbt[3]<-s;}
                    sbtp<-tolower(paste(sbt[sbt!=""],collapse=", "));
                    ylim<-c(0,max(isss,esss,na.rm=TRUE));
                    plotXY(yrs,isss,ylim=ylim,xlab='',ylab="Sample Size",line.type=1);
                    plotXY(yrs,esss,overplot=TRUE,clr="darkblue",line.type=1);
                    mtext(sbtp,side=3,adj=0.01,cex=0.8,line=0)
                    mtext(label,side=3,adj=0.5,cex=0.9,line=1)
                }
            }
        }
    }
}