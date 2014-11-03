#'
#'@title Plot population abundance components.
#'
#'@param res - model results list object
#'@param z.legal - legal size (mm CW)
#'@param clrs - color scheme for plotting
#'
#'@import graphics
#'
#'@export
#'
plotPopComponents.Abundance<-function(res,
                                      z.legal=138,
                                      clrs=c("black","green","blue","grey","cyan")){
    n.xmsyz<-res$pop.quants$n.xmsyz;
    
    n.dims<-dim(n.xmsyz);
    nms.xmsyz<-dimnames(n.xmsyz);
    sxs<-nms.xmsyz[[1]];
    mss<-nms.xmsyz[[2]];
    scs<-nms.xmsyz[[3]]
    yrs<-as.numeric(nms.xmsyz[[4]]);
    zBs<-as.numeric(nms.xmsyz[[5]]);
    
    #get total abundance by year
    n.y<-sumByDims(n.xmsyz,dims=4)
    #get abundance by sex, year
    n.xy<-sumByDims(n.xmsyz,dims=c(1,4));
    #get abundance by sex, maturity state, year
    n.xmy<-sumByDims(n.xmsyz,dims=c(1,2,4));
    #get abundance by sex, maturity state, shell condition, year
    n.xmsy<-sumByDims(n.xmsyz,dims=c(1,2,3,4));
    
    #get total abundance of legal males
    nl.xmsyz<-n.xmsyz[,,,,(zBs>z.legal)];
    nl.xy<-sumByDims(nl.xmsyz,dims=c(1,4));
    
    #plot abundance by sex, year
    plot(yrs,n.y,xlab='',ylab='Abundance (millions)',type='n')
    ctr<-1;
    lines(yrs,n.y,col=clrs[ctr],lwd=3)
    for (x in sxs){
        ctr<-ctr+1;
        lines(yrs,n.xy[x,],col=clrs[ctr],lwd=2);
    }
    legend("topright",c("total",tolower(sxs)),col=clrs,lty=1,cex=0.8)
    
    #plot abundance by sex by maturity state, year
    old.par<-par(mfcol=c(length(sxs),1),mar=c(2,3,1,1));
    for (x in sxs){
        plot(yrs,n.xy[x,],xlab='',ylab='Abundance (millions)',type='n')
        mtext(paste(tolower(x),'s',sep=''),side=3,adj=0,line=-1);
        ctr<-1;
        lines(yrs,n.xy[x,],col=clrs[ctr],lwd=3);
        for (m in mss){
            ctr<-ctr+1;
            lines(yrs,n.xmy[x,m,],col=clrs[ctr],lwd=2);
        }
        legend("topright",c("total",tolower(mss)),col=clrs,lty=1,cex=0.8)
    }
    par(old.par);
    
    #plot abudnance of legal males by year
    x<-"MALE";
    plot(yrs,n.xy[x,],xlab='',ylab='Abundance (millions)',type='n')
    mtext(paste(tolower(x),'s',sep=''),side=3,adj=0,line=-1);
    ctr<-1;
    lines(yrs,n.xy[x,],col=clrs[ctr],lwd=3);
    ctr<-ctr+1;
    lines(yrs,nl.xy[x,],col=clrs[ctr],lwd=2);
    legend("topright",c("total","legal"),col=clrs,lty=1,cex=0.8)
    
}
