#'
#'@title Check handling mortality/fishing equations consistency.
#'
#'@description Function to check handling mortality/fishing equations consistency.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
checkHandlingMortalityConsistency<-function(tcsam=NULL,
                                            rsim=NULL,
                                            showPlot=TRUE,
                                            pdf=NULL,
                                            width=8,
                                            height=6){
    #extract discard biomass
    path<-'mr/F_list/dsB_fyxms';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr$f<-gsub("_"," ",mdfr$f,fixed=TRUE);#replace '_'s in fishery names with spaces
    ds<-dcast(mdfr,modeltype+model+f+y~.,value.var='val',fun.aggregate=sum)
    
    #extract discard mortality biomass
    path<-'mr/F_list/dmB_fyxms';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr$f<-gsub("_"," ",mdfr$f,fixed=TRUE);#replace '_'s in fishery names with spaces
    dm<-dcast(mdfr,modeltype+model+f+y~.,value.var='val',fun.aggregate=sum)
    
    dm$ratio <-dm[['.']]/ds[['.']];
    
    #extract handling mortality rates
    path<-'mp/F_list/hm_fy';
    hm<-getMDFR(path,tcsam,rsim);
    hm$f<-gsub("_"," ",mdfr$f,fixed=TRUE);#replace '_'s in fishery names with spaces
    hm<-hm[hm$val>0,];#include only where non-zero
        
    p <- ggplot(dm,aes(x=y,y=ratio,colour=f,shape=model));
#    p <- p + geom_line()
    p <- p + geom_point(size=3)
    p <- p + guides(colour=guide_legend("fishery"))
    p <- p + geom_line(mapping=aes(y=val),data=hm,linetype=1,size=4,alpha=0.5)
    p <- p + labs(x='year',y='ratio [discard mortality (biomass) : discard biomass]')
    p <- p + ggtitle('gmacs-style Handling Mortality Consistency Check')
    if (showPlot) print(p);
    
    return(invisible(p))
}