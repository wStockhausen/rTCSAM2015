#'
#'@title Plot time series of the fully-selected fishery capture rates.
#'
#'@description Plot time series of the fully-selected fishery capture rates.
#'
#'@param res - results list from a TCSAM2014 model run
#'
#'@export
#'@import graphics
#'
plotFishingRates<-function(res){
    nFsh<-res$mc$nFsh;
    symb<-getPlotSymbology(res);
    for (f in 1:nFsh){
        fnm<-res$mc$lbls.fsh[f];
        Fc.xmsy<-res$fisheries[[f]]$tot$Fc.xmsy;
        dns<-dimnames(Fc.xmsy);
        yrs<-as.numeric(dns[[4]]);
        fmax<-max(Fc.xmsy,na.rm=TRUE);
        plot(range(yrs),c(0,fmax),type='n',xlab="year",ylab="Fishing Capture Rate (yr^-1)");
        for (x in dns[[1]]){
            for (m in dns[[2]]){
                for (s in dns[[3]]){
                    points(yrs,Fc.xmsy[x,m,s,],col=symb$col[x],pch=symb$pch[m]);
                    lines(yrs,Fc.xmsy[x,m,s,],col=symb$col[x],lty=symb$lty[s]);
                }
            }
        }
        mtext(fnm,side=3,adj=0.05);
        li<-symb$leg.info;
        legend("topright",li$txt,col=li$col,lty=li$lty,pch=li$pch,text.col=li$txt.col)
    }
}