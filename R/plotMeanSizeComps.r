#'
#'@title Plot comparison of observed and model size comps averaged over time
#'
#'@param sizebins - 
#'@param obs.comps - 
#'@param prd.comp - 
#'@param cols - 
#'@param ymx - 
#'@param xlab - 
#'@param ylab -
#'@param pch - 
#'@param lty - 
#'@param lwd - 
#'@param clr - 
#'@param CI - 
#'@param bar.width - 
#'@param addToPlot - 
#'
#'@export
#'@import graphics
#'
#source("plotErrorBars.V.r")
plotMeanSizeComps<-function(size.bins,
                            obs.comp,
                            prd.comp,
                            cols=NULL,
                            ymx=NULL,
                            xlab="year",
                            ylab="size mmCW",
                            pch=21,
                            lty=1,
                            lwd=1,
                            clr='black',
                            CI=0.8,
                            bar.width=1,
                            addToPlot=FALSE){
    #set columns to extract
    if (is.null(cols)){cols<-1:ncol(obs.comp)}
    
    #calculate means and std. dev.s by size bin
    mns.obs<-colMeans(obs.comp,na.rm=TRUE)[cols];
    std.obs<-apply(obs.comp,2,sd,na.rm=TRUE)[cols]/sqrt(length(cols));#standard errors of mean
    mns.prd<-colMeans(prd.comp,na.rm=TRUE)[cols];
    std.prd<-apply(prd.comp,2,sd,na.rm=TRUE)[cols]/sqrt(length(cols));#standard errors of mean
    
    if (!addToPlot){
        if (is.null(ymx)){
            ymx<-max(mns.obs+std.obs);
        }
        plot(length.bins, mns.obs,
             type="n",pch=pch,col=clr,
             ylim=c(0,ymx),
             xlab=xlab,ylab=ylab);
    }
    plotErrorBars.V(size.bins,
                    mns.obs,
                    sigma=std.obs,
                    CI=CI,
                    width=bar.width,
                    pch=pch,col=clr)
    points(size.bins,mns.obs,pch=pch,col=clr)
    lines(size.bins,mns.prd,lty=lty,lwd=lwd,col=clr)
}