#'
#'@title Plot initial and final parameter values associated with a ...VectorVector info list object.
#'
#'@description Function to plot initial and final parameter values associated with a ...VectorVector info list object.
#'
#'@param vv - the ...VectorVector info list object
#'@param label - the ...VectorVector variable name
#'@param xlab - the x axis label
#'@param ylab - the y-axis label
#'@param pchs - vector of point symbols to use
#'@param clrs - vector of colors to use
#'
#'@import graphics
#'
#'@export
#'
plotParameters.VectorVector<-function(vv,
                                      label='',
                                      xlab='',
                                      ylab='',
                                      pchs=21:25,
                                      clrs=c("black","blue","green","grey","cyan")){
    npc<-length(vv);#number of pc's to plot
    
    #determine plot scales
    xlim<-NA;
    ylim<-NA;
    for (pc in 1:npc){
        v<-vv[[pc]];
        xv<-as.numeric(names(v$initVals));
        xlim<-range(xlim,xv,na.rm=TRUE,finite=TRUE);
        ylim<-range(ylim,v$initVals,v$finalVals,v$lower,v$upper,na.rm=TRUE,finite=TRUE);
    }
    
    pchs<-rep(pchs,length.out=npc);
    clrs<-rep(clrs,length.out=npc);
    
    #plot devs
    leg.txt<-NA;
    leg.pch<-NA;
    leg.bg <-NA;
    leg.clr<-NA;
    leg.lty<-NA;
    leg.lwd<-NA;
    plot(xlim,ylim,type='n',xlab=xlab,ylab=ylab);
    for (pc in 1:npc){
        v<-vv[[pc]];
        xv<-as.numeric(names(v$initVals));
        xvp<-0.2*(xv[2]-xv[1]);
        yvi<-v$initVals;
        yvf<-v$finalVals;
         if (v$pdfType$type!="none"){
            vp<-seq(from=v$lower,to=v$upper,length.out=100);
            lp<-calcLogPrior(v$pdfType,v=vp,x=xv);
            plotPDFRects(lp,yvi,add=TRUE)
        }
        abline(h=v$lower,col=clrs[pc],lwd=1,lty=3)
        abline(h=v$upper,col=clrs[pc],lwd=1,lty=3)
        points(xv,yvi,col=clrs[pc],pch=pchs[pc],bg=NA,cex=0.8);
        lines( xv,yvi,col=clrs[pc],lty=2,lwd=1);
        points(xv+xvp,yvf,col=clrs[pc],pch=pchs[pc],bg=clrs[pc],cex=0.8);
        lines( xv+xvp,yvf,col=clrs[pc],lty=1,lwd=2);
        leg.txt<-c(leg.txt,paste("PC",pc,"init"),paste("PC",pc,"final"));
        leg.pch<-c(leg.pch,pchs[pc],pchs[pc]);
        leg.bg <-c(leg.bg, NA,clrs[pc]);
        leg.clr<-c(leg.clr,clrs[pc],clrs[pc]);
        leg.lty<-c(leg.lty,2,1);
        leg.lwd<-c(leg.lwd,1,2);
    }
    leg.txt<-leg.txt[!is.na(leg.txt)];
    leg.pch<-leg.pch[!is.na(leg.pch)];
    leg.bg <-leg.bg[ !is.na(leg.bg )];
    leg.clr<-leg.clr[!is.na(leg.clr)];
    leg.lty<-leg.lty[!is.na(leg.lty)];
    leg.lwd<-leg.lwd[!is.na(leg.lwd)];
    legend("bottomleft",cex=0.7,leg.txt,text.col=leg.clr,
           col=leg.clr,pch=leg.pch,pt.bg=leg.bg,lty=leg.lty,lwd=leg.lwd)
    mtext(label,side=3,adj=0.05,line=-1)
}