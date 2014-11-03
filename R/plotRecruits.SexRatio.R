#'
#'@title Plot time series of TCSAM 2015 model sex ratio for recruits
#'
#'@description Plots the recruitment sex ratio time series from a TCSAM 2015 model run.
#'
#'@param res - list object from TCSM 2015 model report file
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
#'@export
#'
plotRecruits.SexRatio<-function(res,
                                xlab="model year",
                                ylab="recruitment sex fraction (males)",
                                xlim=NULL,
                                ylim=c(0,1),
                                clr="blue",
                                pts.plot=TRUE,
                                pts.type=21,
                                pts.siz=8,
                                line.plot=TRUE,
                                line.wd=2,
                                line.type=1,
                                overplot=FALSE){
    yrs<-as.numeric(names(res$pop.quants$Rx.y));
    rx<-as.vector(res$pop.quants$Rx.y);
    plotXY(yrs,rx,
            xlab=xlab,
            ylab=ylab,
            xlim=xlim,
            ylim=ylim,
            clr=clr,
            pts.plot=pts.plot,
            pts.type=pts.type,
            pts.siz=pts.siz,
            line.plot=line.plot,
            line.wd=line.wd,
            line.type=line.type,
            overplot=overplot);
}
