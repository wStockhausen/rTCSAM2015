#'
#'@title Plot time series of TCSAM 2014 model spawning biomass
#'
#'@description Plots the spawning biomass time series from a TCSAM 2014 model run.
#'
#'@param res - results object from TCSM 2014 model output
#'@param logscale - flag (T/F) to plot on ln-scale
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param clr - line/points color
#'@param pts.plot - flag (T/F) to plot points
#'@param pts.type - type of point to plot (pch from 'par')
#'@param pts.siz - point size
#'@param line.plot - flag (T/F) to plot line
#'@param line.wd - line width (lwd from par)
#'@param line.type - type of line (lty from 'par')
#'@param overplot - flag (T/F) to plot the data on an existing plot
#'
#'@details Plots spawning biomass by sex across years in the model run. Characteristics
#'for points and lines (clr, type, and siz/wd) can be vectors.
#'
plotSpB<-function(res,
                    logscale=FALSE,
                    xlab="model year",
                    ylab="spawning biomass (1000's t)",
                    xlim=NULL,
                    ylim=NULL,
                    clr=c("red","blue"),
                    pts.plot=TRUE,
                    pts.type=21,
                    pts.siz=8,
                    line.plot=TRUE,
                    line.wd=2,
                    line.type=c(2,1),
                    overplot=FALSE){
    sxs<-res$mc$SXs;
    n<-length(sxs);
    yrs<-(res$mc$mnYr):(res$mc$mxYr);
    spb.yx<-res$pop.quants$spb.yx;
    if (logscale) spb.yx<-log(spb.yx);
    
    clr<-rep(clr,length.out=n);
    pts.type<-rep(pts.type,length.out=n);
    pts.siz<-rep(pts.siz,length.out=n);
    line.type<-rep(line.type,length.out=n);
    line.wd<-rep(line.wd,length.out=n);
    
    if (is.null(ylim)) {
        if (!logscale) {ylim<-c(0,1.05*max(spb.yx));} 
        else {ylim<-range(spb.yx);}
    }
    
    x<-1;
    if (!overplot) {
        plotXY(yrs,t(spb.yx[,x]),
                xlab=xlab,
                ylab=ylab,
                xlim=xlim,
                ylim=ylim,
                clr=clr[x],
                pts.plot=pts.plot,
                pts.type=pts.type[x],
                pts.siz=pts.siz[x],
                line.plot=line.plot,
                line.wd=line.wd[x],
                line.type=line.type[x],
                overplot=FALSE);
        x<-2;
    }
    if (x<=n){
        for (xp in x:n){
            plotXY(yrs,t(spb.yx[,xp]),
                    xlab=xlab,
                    ylab=ylab,
                    xlim=xlim,
                    ylim=ylim,
                    clr=clr[xp],
                    pts.plot=pts.plot,
                    pts.type=pts.type[xp],
                    pts.siz=pts.siz[xp],
                    line.wd=line.wd[xp],
                    line.type=line.type[xp],
                    overplot=TRUE);
        }
    }
    if (line.plot&&pts.plot) {
        legend("topright",sxs,col=clr,pch=pts.type,lty=line.type,lwd=line.wd)
    } else if (line.plot) {
        legend("topright",sxs,col=clr,lty=line.type,lwd=line.wd)
    } else {
        legend("topright",sxs,col=clr,pch=pts.type)
    }
}
