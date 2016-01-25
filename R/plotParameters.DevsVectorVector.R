#'
#'@title Plot initial and final parameter values associated with a DevsVectorVector info list object.
#'
#'@description Function to plot initial and final parameter values associated with a DevsVectorVector info list object.
#'
#'@param dvv - the DevsVectorVector info list object
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@export
#'
plotParameters.DevsVectorVector<-function(dvv,
                                          verbose=FALSE){
    npc<-length(dvv);#number of pc's to plot
    
    #determine plot scales
    xlim<-NA;
    ylim<-NA;
    for (pc in 1:npc){
        dv<-dvv[[pc]];
        xv<-as.numeric(names(dv$initVals));
        yvi<-dv$initVals;
        yvf<-dv$finalVals;
        xlim<-range(xlim,xv,na.rm=TRUE,finite=TRUE);
        ylim<-range(ylim,yvi,yvf,na.rm=TRUE,finite=TRUE);
    }
    
    #plot devs
    plot(xlim,ylim,type='n',xlab='',ylab='');
    for (pc in 1:npc){
        dv<-dvv[[pc]];
        xv<-as.numeric(names(dv$initVals));
        xvp<-0.2*(xv[2]-xv[1]);
        yvi<-dv$initVals;
        yvf<-dv$finalVals;
        points(xv,yvi);
        lines(xv,yvi);
        points(xv+xvp,yvf)
        lines(xv+xvp,yvf)
    }
}