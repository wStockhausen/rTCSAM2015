#'
#'@title Plot comparisons of model and data size compositions.
#'
#'@description Function to plot comparisons of model and data size compositions.
#'
#'@param name - name of size comps source
#'@param od - observed size comps list object (may be NULL)
#'@param md - model (predicted) size comps list object (may be NULL)
#'@param label - label for title
#'@param ncol - number of columns per page for plots
#'@param showPlots - flag (T/F) to show plots
#'
#'@return list of ggplot objects comprising the graph pages to plot
#'
#'@import ggplot2
#'
plotSizeCompsComparisons0<-function(name,
                                    od,
                                    md,
                                    normalize=TRUE,
                                    label='',
                                    ncol=2,
                                    showPlots=TRUE){
    #define dimension variable names
    varnames<-c("sx","ms","sc","year","size");
    
    #reshape observed size comps
    if (!is.null(od)){
        od.data<-od$data;
        if (normalize){
            dn<-dimnames(od.data);
            usx<-dn[[1]];
            ums<-dn[[2]];
            usc<-dn[[3]];
            yrs<-dn[[4]];
            for (sxp in usx){
                for (msp in ums){
                    for (scp in usc){
                        for (y in yrs){
                            tot<-sum(od.data[sxp,msp,scp,y,],na.rm=TRUE);
                            od.data[sxp,msp,scp,y,]<-od.data[sxp,msp,scp,y,]/tot;
                        }
                    }
                }
            }            
        }
        obs<-reshape2::melt(od.data,varnames=varnames,value.name='N');
        obs$type<-'observed';#add 'type' column
    } else obs<-NULL;
    
    #reshape model-predicted size comps
    if (!is.null(md)){
        md.data<-md$data;
        if (normalize){
            dn<-dimnames(md.data);
            usx<-dn[[1]];
            ums<-dn[[2]];
            usc<-dn[[3]];
            yrs<-dn[[4]];
            for (sxp in usx){
                for (msp in ums){
                    for (scp in usc){
                        for (y in yrs){
                            tot<-sum(md.data[sxp,msp,scp,y,],na.rm=TRUE);
                            if (tot>0) md.data[sxp,msp,scp,y,]<-md.data[sxp,msp,scp,y,]/tot;
                        }
                    }
                }
            }            
        }
        mod<-reshape2::melt(md.data,varnames=varnames,value.name='N');
        mod$type<-'predicted';#add 'type' column
    } else mod<-NULL;
    
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
                    p <- p + ggtitle(paste(label,': ',name,": ",tolower(paste(sxp,msp,scp,sep=', ')),sep=''))
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