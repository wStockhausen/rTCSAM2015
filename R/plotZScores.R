#'
#'@title Plot z-scores from fits to data
#'
#'@param afits - 
#'@param xlim - x axis limits (computed internally if NULL)
#'@param ylim - y axis limits (computed internally if NULL)
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param label - plot label
#'
#'@details Standard graphics approach to plotting z-scores.
#'
plotZScores<-function(afits,
                      xlim=NULL,
                      ylim=NULL,
                      xlab="years",
                      ylab="z-scores",
                      label=""){
    types.fits<-names(afits);
    for (type in types.fits){
        if(!is.null(afits[[type]])){
            nll<-afits[[type]]$nll;
            zscrs<-afits[[type]]$zscrs;
            yrs<-as.numeric(names(zscrs))
            plot(yrs,as.vector(zscrs),xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim)
            lines(yrs,0*yrs,col='black',lty=2);
            mtext(label,side=3,adj=0.05,cex=0.8);
            mtext(paste("nll:",nll),side=3,adj=0.95,cex=0.7);
            mtext(type,side=3,adj=0.05,cex=0.7,line=-1);
        }
    }
}
