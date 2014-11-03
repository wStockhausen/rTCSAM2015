#'
#'@title Plot population biomass components.
#'
#'@param res - model results list object
#'@param z.legal - legal size (mm CW)
#'@param clrs - color scheme for plotting
#'
#'@import graphics
#'
#'@export
#'
plotPopComponents.Biomass<-function(res,
                                     z.legal=138,
                                     clrs=c("black","green","blue","grey","cyan")){
    n.xmsyz<-res$pop.quants$n.xmsyz;  #abundance in millions
    wAtZ.xmz<-res$data$bio$wAtZ$data; #weight-at-size (kg)
    
    n.dims<-dim(n.xmsyz);
    nms.xmsyz<-dimnames(n.xmsyz);
    sxs<-nms.xmsyz[[1]];
    mss<-nms.xmsyz[[2]];
    scs<-nms.xmsyz[[3]];
    yrs<-as.numeric(nms.xmsyz[[4]]);
    zBs<-as.numeric(nms.xmsyz[[5]]);
    
    w.xmsyz<-0*n.xmsyz;
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                for (y in nms.xmsyz[[4]]){
                    w.xmsyz[x,m,s,y,]<-n.xmsyz[x,m,s,y,]*wAtZ.xmz[x,m,];
                }
            }    #plot biomass by sex by ma
        }
    }
    
    #get total biomass by year
    w.y<-sumByDims(w.xmsyz,dims=4)
    #get biomass by sex, year
    w.xy<-sumByDims(w.xmsyz,dims=c(1,4));
    #get biomass by sex, maturity state, year
    w.xmy<-sumByDims(w.xmsyz,dims=c(1,2,4));
    #get biomass by sex, maturity state, shell condition, year
    w.xmsy<-sumByDims(w.xmsyz,dims=c(1,2,3,4));
    
    #get total biomass of legal males
    wl.xmsyz<-w.xmsyz[,,,,(zBs>z.legal)];
    wl.xy<-sumByDims(wl.xmsyz,dims=c(1,4));
    
    #plot biomass by sex, year
    plot(yrs,w.y,xlab='',ylab="Biomass (1000's t)",type='n')
    ctr<-1;
    lines(yrs,w.y,col=clrs[ctr],lwd=3)
    for (x in sxs){
        ctr<-ctr+1;
        lines(yrs,w.xy[x,],col=clrs[ctr],lwd=2);
    }
    legend("topright",c("total",tolower(sxs)),col=clrs,lty=1,cex=0.8)
    
    #plot biomass by sex by maturity state, year
    old.par<-par(mfcol=c(length(sxs),1),mar=c(2,4,1,1));
    for (x in sxs){
        plot(yrs,w.xy[x,],xlab='',ylab="Biomass (1000's t)",type='n')
        mtext(paste(tolower(x),'s',sep=''),side=3,adj=0,line=-1);
        ctr<-1;
        lines(yrs,w.xy[x,],col=clrs[ctr],lwd=3);
        for (m in mss){
            ctr<-ctr+1;
            lines(yrs,w.xmy[x,m,],col=clrs[ctr],lwd=2);
        }
        legend("topright",c("total",tolower(mss)),col=clrs,lty=1,cex=0.8)
    }
    par(old.par);
    
    #plot biomass of legal males by year
    x<-"MALE";
    plot(yrs,w.xy[x,],xlab='',ylab="Biomass (1000's t)",type='n')
    mtext(paste(tolower(x),'s',sep=''),side=3,adj=0,line=-1);
    ctr<-1;
    lines(yrs,w.xy[x,],col=clrs[ctr],lwd=3);
    ctr<-ctr+1;
    lines(yrs,wl.xy[x,],col=clrs[ctr],lwd=2);
    legend("topright",c("total","legal"),col=clrs,lty=1,cex=0.8)
    
}
