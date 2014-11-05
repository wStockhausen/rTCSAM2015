#'
#'@title Plot comparisons of model and data size compositions by year.
#'
#'@description Function to plot comparisons of model and data size compositions by year. Males are plotted
#'as positive frequencies, females as negative frequencies, and maturity/shell condition combinations 
#'disntiguished by different colors.
#'
#'@param name - name of size comps source
#'@param od - observed size comps list object (may be NULL)
#'@param md - model (predicted) size comps list object (may be NULL)
#'@param sxs - vector of sexes to plot ("MALE", "FEMALE", "ALL_SEX")
#'@param ms.sc - dataframe with columns ms (maturity) and sc (shell condition) listing combinations to plot
#'@param ncol - number of columns per page for plots
#'@param nrow - number of rows per page for plots
#'@param showPlots - flag (T/F) to show plots
#'
#'@return list of ggplot objects comprising the graph pages to plot
#'
#'@importFrom sqldf sqldf
#'@importFrom reshape2 melt
#'@import ggplot2
#'
#'@export
#'
plotSizeCompsComparisons1<-function(name,
                                    od,
                                    md,
                                    sxs=c("MALE","FEMALE"),
                                    ms.sc=as.data.frame(list(ms=c("IMMATURE","MATURE","MATURE"),
                                                             sc=c("NEW_SHELL","NEW_SHELL","OLD_SHELL")),
                                                        stringsAsFactors=FALSE),
                                    ncol=2,
                                    nrow=5,
                                    showPlots=TRUE){
    
    #create vector of maturity x shell condition states
    ms.scs<-tolower(paste(ms.sc$ms,ms.sc$sc,sep=', '))
    
    #define dimension variable names
    varnames<-c("sx","ms","sc","year","size");
    
    #reshape observed size comps
    if (!is.null(od)){
        obs<-reshape2::melt(od$data,varnames=varnames,value.name='N');
        obs$type<-'observed';#add 'type' column
    } else obs<-NULL;
    
    #reshape model-predicted size comps
    if (!is.null(md)){
        mod<-reshape2::melt(md$data,varnames=varnames,value.name='N');
        mod$type<-'predicted';#add 'type' column
    } else mod<-NULL;
    
    dfr<-rbind(obs,mod);
    dfr$ms_sc<-tolower(paste(dfr$ms,dfr$sc,sep=', '))
    dfr<-dfr[(dfr$sx %in% sxs)&(dfr$ms_sc %in% ms.scs),]
    idx<-dfr[['sx']]=='FEMALE';
    dfr[idx,'N']<- -1*dfr[idx,'N'];
    
    qry<-"select * from dfr
          order by type,year,ms_sc,sx,size;"
    dfr<-sqldf(qry);
    
    uz<-unique(dfr$size);
    rng<-range(dfr$N);
    cat("rng = ",rng,'\n')
    
    yrs<-unique(dfr$year);
    mxp<-nrow*ncol;
    npg<-ceiling(length(yrs)/mxp)
    
    ps<-list();
    for (pg in 1:npg){
        dfrp<-dfr[dfr$year %in% yrs[(pg-1)*mxp+1:mxp],]
        p <- ggplot(data=dfrp)
        p <- p + geom_bar(aes(x=size,y=N,fill=ms_sc),data=dfrp[dfrp$type=='observed',],stat="identity",position='identity',alpha=0.5)
        for (sxp in sxs){
            for (ms.scp in ms.scs){
                p <- p + geom_line(aes(x=size,y=N,colour=ms_sc),data=dfrp[(dfrp$type=='predicted')&(dfrp$sx==sxp)&(dfrp$ms_sc==ms.scp),],size=1)
            }
        }
        p <- p + scale_x_continuous(breaks=pretty(uz)) 
        p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0))
        p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
        p <- p + labs(x="Size (mm)",y="proportion ")
        p <- p + facet_wrap(~year,ncol=2) 
        p <- p + ggtitle(gsub('.',' ',name,fixed=TRUE))
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
        p <- p + ggtheme
        if (showPlots) print(p);
        ps[[pg]]<-p
    }
    return(ps)
}

#ps<-plotSizeCompsComparisons1(name,od,md)
#ps<-plotSizeCompsComparisons1(name,NULL,md)
