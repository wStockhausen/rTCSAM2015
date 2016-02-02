#'
#'@title Calculate and plot a posterior density based on an MCMC data vector.
#'
#'@description Function to calculate and plot a posterior density based on an MCMC data vector.
#'
#'@param data - the MCMC data from which to estimate the posterior density.
#'@param scaleBy - factor to scale data by
#'@param plotEst - flag (T/F) to plot the MLE estimate (assumed to be 1st value)
#'@param add - flag (T/F) to add to existing plot (creates new plot if FALSE)
#'@param clr - color for density plot
#'@param alpha - transparency value to apply to clr
#'@param xlim - x axis limits (if add=FALSE)
#'@param ylim - y axis limits (if add=FALSE)
#'@param xlabel - label for x axis (if add=FALSE)
#'@param label - label for plot (if add=FALSE)
#'
#'@details Uses functions
#'\itemize{
#'  \item wtsUtilities::addTransparency(...)
#'}
#'
#'@export
#'
plotMCMCDensity<-function(data,
                          scaleBy=1,
                          plotEst=TRUE,
                          add=TRUE,
                          clr="cyan",
                          alpha=0.25,
                          xlim=NULL,
                          ylim=NULL,
                          xlabel='',
                          label=''){
    if (!is.vector(data)){
        cat('Error in plotMCMCDensity:\n',
            'data must be a vector!\n',
            'Returning NULL...\n');
        return(NULL);
    }
    d<-density(data);
    d.x<-d$x/scaleBy;
    d.y<-d$y/max(d$y,na.rm=TRUE);
    cat('max(d.y)=',max(d.y),"\n");
    if (!add){
        if (is.null(xlim)) xlim<-range(d.x,na.rm=TRUE,finite=TRUE);
        if (is.null(ylim)) ylim<-range(d.y,na.rm=TRUE,finite=TRUE)
        plot(xlim,ylim,type='n',
             xlim=xlim,xlab=xlabel,
             ylim=ylim,ylab='',yaxt='n');
        mtext(label,side=3,line=-1,adj=0.01,cex=0.8)
    }
    clr.a<-wtsUtilities::addTransparency(clr,alpha);
    polygon(c(d.x[1],d.x,d.x[length(d.x)],d.x[1]),c(0,d.y,0,0),col=clr.a);
    lines(d.x,d.y,col=clr,lty=1,lwd=2);
    if (plotEst){
        abline(v=data[1],col=clr,lty=1,lwd=3)
    }
    return(invisible(d));
}