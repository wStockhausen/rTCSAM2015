#'
#'@title Calculate and plot a posterior density based on a 3d MCMC data aray.
#'
#'@description Function to calculate and plot posterior densities based on a 3d MCMC data array.
#'
#'@param data - the MCMC data array from which to estimate the posterior densities.
#'@param scaleBy - factor to scale data by
#'@param plotEst - flag (T/F) to plot the MLE estimate (assumed to be 1st value)
#'@param add - flag (T/F) to add to existing plot (creates new plot if FALSE)
#'@param colorscale - color scale to use for the density plot
#'@param alpha - transparency value to apply to the colorscale
#'@param xlim - x axis limits (if add=FALSE)
#'@param ylim - y axis limits (if add=FALSE)
#'@param xlabel - label for x axis (if add=FALSE)
#'@param label - label for plot (if add=FALSE)
#'
#'@details Uses functions
#'\itemize{
#'  \item wtsUtilities::createColorScale(...)
#'}
#'
#'@import graphics
#'
#'@export
#'
plotMCMCResults.forArray3D<-function(data,
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
    if (!is.array(data)){
        cat('Error in plotMCMCResults.forArray3D:\n',
            'data must be a 3d array!\n',
            'Returning NULL...\n');
        return(NULL);
    }
    dms.n<-dim(data);
    if (length(dms.n)!=3){
        cat('Error in plotMCMCResults.forArray3D:\n',
            'data must be a 3d array!\n',
            'Returning NULL...\n');
        return(NULL);
    }
    
    dms.nms<-dimnames(data);
    facs<-dms.nms[[1]];
    ds2<-vector(mode='list',length=length(facs));
    names(ds2)<-facs;
    
    getXLim<-FALSE;
    if (is.null(xlim)) {getXLim<-TRUE; xlim<-NA;}
    getYLim<-FALSE;
    if (is.null(ylim)) {getYLim<-TRUE; ylim<-NA;}
    
    for (fac in facs){
        ds1<-plotMCMCResults.forMatrix(data[fac,,],scaleBy=scaleBy,doPlot=FALSE);
        if (getXLim) xlim<-range(xlim,ds1$xlim,na.rm=TRUE,finite=TRUE);
        if (getYLim) ylim<-range(ylim,ds1$ylim,na.rm=TRUE,finite=TRUE);
        ds2[[fac]]<-ds1$ds1;
    }
    
    if (doPlot){
        scale<-wtsUtilities::createColorScale(name=colorscale[1]);
        
        if (!add) plot(xlim,ylim,xlab=xlabel,ylab=ylabel,type='n');
        for (fac in facs){
            ds1<-ds2[[fac]];
            xs<-as.numeric(names(ds1))
            nx<-length(xs);
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
                lines(xs,data[fac,,1],lty=1,lwd=2);
                points(xs,data[fac,,1],pch=21,cex=0.8)
            }   
        }
        mtext(label,side=3,adj=0.01,cex=0.8,line=-1)
    }
    
    return(invisible(list(ds2=ds2,xlim=xlim,ylim=ylim)))
}