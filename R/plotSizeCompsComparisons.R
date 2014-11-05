#'
#'@title Plot comparisons of model and data size compositions.
#'
#'@description Function to plot comparisons of model and data size compositions.
#'
#'@param name - name of size comps source
#'@param od - observed size comps list object (may be NULL)
#'@param md - model (predicted) size comps list object (may be NULL)
#'@param ncol - number of columns per page for plots
#'@param showPlots - flag (T/F) to show plots
#'
#'@return list of ggplot objects comprising the graph pages to plot
#'
#'@importFrom reshape2 melt
#'@import ggplot2
#'
plotSizeCompsComparisons<-function(name,od,md,
                                   ncol=2,
                                   showPlots=TRUE){
    #reshape observed size comps
    varnames<-c("sx","ms","sc","year","size");
    obs<-reshape2::melt(od$data,varnames=varnames,value.name='N');
    
    #reshape model-predicted size comps
    varnames<-c("sx","ms","sc","year","size");
    mod<-reshape2::melt(md$data,varnames=varnames,value.name='N');
    
    usx<-unique(obs$sx);
    ums<-unique(obs$ms);
    usc<-unique(obs$sc);
    uz<-unique(obs$size);
    
    ctr<-0;
    ps<-list();
    for (sxp in usx){
        for (msp in ums){
            for (scp in usc){
                sbo<-obs[(sxp == obs$sx)&(msp == obs$ms)&(scp == obs$sc),]
                sbm<-mod[(sxp == obs$sx)&(msp == obs$ms)&(scp == obs$sc),]
                if ((!is.null(sbo))&(sum(sbo$N,na.rm=TRUE)>0)){
                    p <- ggplot(data=sbo)
                    p <- p + geom_bar(aes(size,N),stat="identity")
                    p <- p + geom_line(aes(size,N),data=sbm,col="red")
                    p <- p + scale_x_discrete(breaks=pretty(uz)) 
                    p <- p + labs(x="Size (mm)",y="proportion ")
                    p <- p + facet_wrap(~year,ncol=ncol) 
                    p <- p + ggtitle(tolower(paste(sxp,msp,scp,sep=', ')))
                    p <- p + ggtheme
                    if (showPlots) print(p);
                    ctr<-ctr+1;
                    ps[[ctr]]<-p;
                }
            }
        }
    }
    return(ps)
}