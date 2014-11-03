#'
#'@title Convenience function to make an x-y plot
#'
#'@description A convenience function to make x-y plots
#'
#'@param x - vector of x coordinates
#'@param y - vector of y coordinates
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param clr  - line/points color
#'@param pts.plot - flag (T/F) to plot points
#'@param pts.type - type of point to plot (pch from 'par')
#'@param pts.siz - point size
#'@param line.plot - flag (T/F) to plot line
#'@param line.wd - line width (lwd from par)
#'@param line.type - type of line (lty from 'par')
#'@param overplot - flag (T/F) to plot the data on an existing plot
#'
#'@import graphics
#'
#'@export
#'
plotXY<-function(x,y,
                  xlab=NULL,
                  ylab=NULL,
                  xlim=NULL,
                  ylim=NULL,
                  clr="blue",
                  pts.plot=TRUE,
                  pts.type=21,
                  pts.siz=8,
                  line.plot=TRUE,
                  line.wd=2,
                  line.type=1,
                  overplot=FALSE){
    
    if (!overplot) {
        plot(x,y,type='n',xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim);
    }
    if (pts.plot) points(x,y,pch=pts.type,col=clr);
    if (line.plot) lines(x,y,lty=line.type,lwd=line.wd,col=clr);
}