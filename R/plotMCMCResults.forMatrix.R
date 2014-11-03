#'
#'@title Calculate and plot a posterior density based on a MCMC data matrix.
#'
#'@description Function to calculate and plot posterior densities based on a MCMC data matrix.
#'
#'@param data - the MCMC data matrix from which to estimate the posterior densities.
#'@param scaleBy - factor to scale data by
#'@param doPlot - flag (T/F) to plot the results
#'@param plotEst - flag (T/F) to plot the MLE estimate (assumed to be 1st value)
#'@param add - flag (T/F) to add to existing plot (creates new plot if FALSE)
#'@param colorscale - color scale to use for the density plot
#'@param alpha - transparency value to apply to the colorscale
#'@param xlim - x axis limits (if add=FALSE)
#'@param ylim - y axis limits (if add=FALSE)
#'@param xlabel - label for x axis (if add=FALSE)
#'@param label - label for plot (if add=FALSE)
#'
#'@import graphics
#'@importFrom wtsUtilities createColorScale
#'
#'@export
#'
plotMCMCResults.forMatrix<-function(data,
                                    scaleBy=1,
                                    doPlot=TRUE,
                                    plotEst=FALSE,
                                    add=TRUE,
                                    colorscale=c("coldhot","hot","cold","jet",NULL),
                                    alpha=0.25,
                                    xlim=NULL,
                                    ylim=NULL,
                                    xlabel='',
                                    ylabel='',
                                    label=''){
    if (!is.matrix(data)){
        cat('Error in plotMCMCResults.forMatrix:\n',
            'data must be a matrix!\n',
            'Returning NULL...\n');
        return(NULL);
    }

    xs<-as.numeric(rownames(data));
    nx<-length(xs);
    
    if (is.null(xlim)) {xlim<-range(xs,na.rm=TRUE,finite=TRUE);}
    getYLim<-FALSE;
    if (is.null(ylim)) {getYLim<-TRUE; ylim<-NA;}

    ds1<-vector(mode='list',length=nx);
    names(ds1)<-rownames(data);
    for (x in 1:nx){
        ds1[[x]]<-density(data[x,]);
        if (getYLim) ylim<-range(ylim,ds1[[x]]$x/scaleBy,na.rm=TRUE,finite=TRUE);
    }
    
    if (doPlot){
        scale<-wtsUtilities::createColorScale(name=colorscale[1]);
        
        if (!add) plot(xlim,ylim,xlab=xlabel,ylab=ylabel,type='n');
        xp<-getIntervalLimits(xs);
        for (x in 1:nx){
            d<-ds1[[x]];
            ny<-length(d$x);
            ys<-d$x/scaleBy;
            zs<-d$y/max(d$y,na.rm=TRUE);
            
            yi<-seq(from=ys[1],to=ys[ny],length.out=100);
            zi<-stats::spline(ys,zs,xout=yi)$y;
            zi<-zi/max(zi,na.rm=TRUE);
            
            ny<-length(yi);
            yp<-getIntervalLimits(yi);
            zclrs<-scale(zi,mn=0,mx=1,alpha=alpha);
            rect(xp[x],yp[1:ny],xp[x+1],yp[(1:ny)+1],col=zclrs,lty=0)
        }
        if (plotEst){
            lines(xs,data[,1],lty=1,lwd=2);
            points(xs,data[,1],pch=21,cex=0.8)
        }
        mtext(label,side=3,adj=0.01,cex=0.8,line=-1)
    }
    
    return(invisible(list(ds1=ds1,xlim=xlim,ylim=ylim)));
}