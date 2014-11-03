#'
#'@title Plot a pdfRects object on an existing plot.
#'
#'@description Function to plot an objec of class 'pdfRects' on an existing plot.
#'
#'@param o - the pdfRects object
#'@param colorscale - the name of the colorscale to use
#'@param alpha - the transparency level for the resulting image
#'
#'@importFrom wtsUtilities createColorScale
#'@import graphics
#'
#'@export
#'
plotPDFRects<-function(o,y=NULL,colorscale='jet',alpha=0.5,add=TRUE){
    nx<-length(o$xl);
    nlp<-length(o$lp);
    
    if (is.null(y)) y<-0*xl;
    
    if (!add){
        xlm<-range(o$xl,o$xr,na.rm=TRUE,finite=TRUE);
        ylm<-range(min(o$vl,na.rm=TRUE,finite=TRUE)+min(y,na.rm=TRUE,finite=TRUE),
                   max(o$vu,na.rm=TRUE,finite=TRUE)+max(y,na.rm=TRUE,finite=TRUE),
                   na.rm=TRUE,finite=TRUE)
        plot(xlm,ylm,type='n',xlim=xlm,ylim=ylm,xlab='',ylab='');
    }
    
    pr<-exp(o$lp);
    pr.rng<-range(pr,na.rm=TRUE,finite=TRUE);
    scale<-wtsUtilities::createColorScale(name=colorscale);
    clrs<-scale(pr,mn=pr.rng[1],mx=pr.rng[2],alpha=alpha)
    for (ix in 1:nx){
        rect(o$xl[ix],o$vl+y[ix],o$xr[ix],o$vu+y[ix],col=clrs,border=NA)
    }
}