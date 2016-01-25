#'
#'@title Plot initial and final parameter values associated with a ...NumberVector info list object.
#'
#'@description Function to plot initial and final parameter values associated with a ...NumberVector info list object.
#'
#'@param vv - the ...NumberVector info list object
#'@param label - the ...NumberVector variable name
#'@param mcmc - mcmc results corresponding to vv
#'@param plotConsts - flag (T/F) to plot parameters even though they were not estimated
#'@param xlab - the x axis label
#'@param ylab - the y-axis label
#'@param pchs - vector of point symbols to use
#'@param clrs - vector of colors to use
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@export
#'
plotParameters.NumberVector<-function(nv,
                                      label='',
                                      mcmc=NULL,
                                      delta=0.005,
                                      plotConsts=FALSE,
                                      xlab='',
                                      ylab='',
                                      pchs=21:25,
                                      clrs=c("black","blue","green","grey","cyan"),
                                      verbose=FALSE){
    
    npc<-length(nv);#number of pc's to plot
    
    trGreen<-wtsUtilities::addTransparency("green",alpha=0.25);
    
    for (pc in 1:npc){
        pcc<-as.character(pc);
        ni<-nv[[pc]];
        if (plotConsts|(ni$phase>0)){
            cat('\tPlotting ',label,'[',pc,']\n',sep='')
            xv<-NA;
            if (ni$pdfType$type!="none"){
                xv<-seq(from=ni$lower,to=ni$upper,length.out=100);
                prior<-exp(calcLogPrior(ni$pdfType,v=xv));
                prior<-prior/max(prior,na.rm=TRUE); #normalize to max = 1 for plotting
                cat('max(prior) = ',max(prior),"\n")
            }
            xm<-NA;
            if (!is.null(mcmc)){
                if (!is.null(mcmc[[pcc]])) xm<-range(mcmc[[pcc]],na.rm=TRUE,finite=TRUE);
            }
            clr<-"black";#default color for parameter estimate
            if (is.null(ni$lower)){
                #unbounded numbers
                xlim<-range(c(0.8,1.2)*ni$initVal,c(0.8,1.2)*ni$finalVal,xv,xm,na.rm=TRUE,finite=TRUE);
            } else {
                #bounded numbers
                if (abs(ni$final-ni$lower)/(ni$upper-ni$lower)<delta){
                    clr<-"blue";
                } else if (abs(ni$upper-ni$final)/(ni$upper-ni$lower)<delta){
                    clr<-"red"
                }
                xlim<-c(ni$lower,ni$upper);
            }
            
            plot(xlim,c(0,1),type='n',xlab='',ylab='',yaxt='n',cex.axis=0.8)
            if (ni$pdfType$type!="none") polygon(c(xv[1],xv,xv[100]),c(0,prior,0),col=trGreen);               
            if (!is.null(mcmc)){
                if (!is.null(mcmc[[pcc]])) plotMCMCDensity(mcmc[[pcc]],clr='cyan',alpha=0.50,add=TRUE)
            }
            if (!is.null(ni$lower)) abline(v=c(ni$lower,ni$upper),lty=2,col='green');#upper, lower limits
            abline(v=ni$initVal,lty=2,lwd=2,col="grey");
            abline(v=ni$finalVal,lty=1,lwd=3,col=clr);            
            mtext(paste(label,"[",pc,"]",sep=''),side=3,adj=0.05,line=-1,cex=0.9);
        }#if
    }#pc loop
    
}