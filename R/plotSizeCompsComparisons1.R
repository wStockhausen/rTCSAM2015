#'
#'@title Plot comparisons of model and data size compositions by year.
#'
#'@description Function to plot comparisons of model and data size compositions by year. Males are plotted
#'as positive frequencies, females as negative frequencies, and maturity/shell condition combinations 
#'distinguished by different colors.
#'
#'@param name - name of size comps source
#'@param od - observed size comps list object (may be NULL)
#'@param md - model (predicted) size comps list object (may be NULL)
#'@param label - label for title
#'@param ncol - number of columns per page for plots
#'@param nrow - number of rows per page for plots
#'@param showPlots - flag (T/F) to show plots
#'
#'@return list of ggplot objects comprising the graph pages to plot
#'
#'@import ggplot2
#'
#'@export
#'
plotSizeCompsComparisons1<-function(name,
                                    od,
                                    md,
                                    label='',
                                    normalize=TRUE,
                                    ncol=2,
                                    nrow=5,
                                    showPlots=TRUE){
    
    #define dimension variable names
    varnames<-c("sx","ms","sc","year","size");
    
    #reshape observed size comps
    if (!is.null(od)){
        obs<-reshape2::melt(od$data,varnames=varnames,value.name='N');
        obs$type<-'observed';#add 'type' column
        
        qry<-'select sx,ms,sc,sum(N) as Nt
              from obs
              group by sx,ms,sc
              order by sx,ms,sc;';
        obs1<-sqldf::sqldf(qry);
        
        qry<-'select sx,ms,sc
              from obs1
              where Nt>0
              order by sx,ms,sc;';
        fcs<-sqldf::sqldf(qry);
    } else obs<-NULL;
    
    #reshape model-predicted size comps
    if (!is.null(md)){
        mod<-reshape2::melt(md$data,varnames=varnames,value.name='N');
        mod$type<-'predicted';#add 'type' column
        
        if (is.null(obs)){
            qry<-'select sx,ms,sc,sum(N) as Nt
                  from mod
                  group by sx,ms,sc
                  order by sx,ms,sc;';
            mod1<-sqldf::sqldf(qry);
            
            qry<-'select sx,ms,sc
                  from mod1
                  where Nt>0
                  order by sx,ms,sc;';
            fcs<-sqldf::sqldf(qry);
        }
    } else mod<-NULL;
    
    dfr<-rbind(obs,mod);
    
    qry<-"select d.type,d.sx,d.ms,d.sc,d.year,d.size,d.N
          from dfr d, fcs f
          where d.sx=f.sx and d.ms=f.ms and d.sc=f.sc
          order by d.type,d.year,d.ms,d.sc,d.sx,d.size;"
    dfr<-sqldf::sqldf(qry);
    
#    obsp<-getSizeCompFitOption();
    dfr$ms_sc<-paste(dfr$ms,dfr$sc,sep=', ')
    
    if (normalize){
        qry<-'select type,sx,year,sum(N) as Nt
              from dfr
              group by type,sx,year
              order by type,sx,year;';
        tots<-sqldf::sqldf(qry);
        
        qry<-'select
                d.type,d.sx,d.ms,d.sc,d.ms_sc,d.year,d.size,d.N/t.Nt as N
              from dfr d, tots t
              where d.type=t.type and d.sx=t.sx and d.year=t.year
              order by d.type,d.year,d.ms,d.sc,d.sx,d.size;';
        dfr<-sqldf::sqldf(qry);
    }
    
    sxs<-unique(dfr$sx)
    ms.scs<-unique(dfr$ms_sc)
    yrs<-unique(dfr$year);
    uz<-unique(dfr$size);
    
    mxp<-nrow*ncol;
    npg<-ceiling(length(yrs)/mxp)
    
    idx<-dfr[['sx']]=='FEMALE';
    dfr[idx,'N']<- -1*dfr[idx,'N'];
    
    rng<-range(dfr$N,na.rm=TRUE,finite=TRUE);
    cat("rng = ",rng,'\n')
    
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
        p <- p + ggtitle(paste(label,': ',name,sep=''))
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
        p <- p + ggtheme
        if (showPlots) print(p);
        ps[[pg]]<-p
    }
    return(ps)
}

#ps<-plotSizeCompsComparisons1(name,od,md)
#ps<-plotSizeCompsComparisons1(name,NULL,md)
