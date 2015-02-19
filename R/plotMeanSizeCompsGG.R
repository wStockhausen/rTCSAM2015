#'
#'@title Plot comparisons of mean model and data size compositions by year and sx.
#'
#'@description Function to plot comparisons of mean model and data size compositions by year for
#'specified sexes. Maturity/shell condition combinations
#'for a given year x sx combination are distinguished by different colors.
#'
#'@param name - name of size comps source
#'@param od - observed size comps list object (may be NULL)
#'@param md - model (predicted) size comps list object (may be NULL)
#'@param label - label for title
#'@param disAggBy - vector of categories to keep disaggregated ('sx','ms',and/or 'sc') or NULL to aggregate over all
#'@param normalize - flag (T/F) to normalize size comps
#'@param normBy - vector of categories to normalize by ('sx','ms',and/or 'sc') or NULL to normalize over all
#'@param ggtheme - ggplot2 theme to use
#'@param showPlot - flag (T/F) to show plots immediately
#'
#'@return list of ggplot objects comprising the graph pages to plot
#'
#'@importFrom sqldf sqldf
#'@importFrom reshape2 melt
#'@import ggplot2
#'
#'@export
#'
plotMeanSizeCompsGG<-function(name,
                              od,
                              md,
                              label='',
                              disAggBy=c('sx','ms','sc'),
                              normalize=TRUE,
                              normBy=NULL,
                              ggtheme=theme_grey(),
                              showPlot=FALSE){
    
    #redefine dimension variable names for convenience
    varnames<-c("sx","ms","sc","year","size");
    
    #reshape observed size comps
    if (!is.null(od)){
        obs<-reshape2::melt(od$data,varnames=varnames,value.name='N');
        
        #aggregate comps as requested
        qry<-'select &&disAggByStr,sum(N) as N
              from obs
              group by &&disAggByStr
              order by &&disAggByStr';
        disAggByStr<-paste(paste(disAggBy,collapse=', '),'year, size',sep=', ')
        qry<-gsub('&&disAggByStr',disAggByStr,qry);
        obs<-sqldf::sqldf(qry);
        if (!('sx' %in% disAggBy)) obs$sx<-'ALL_SEX';      #sx was aggregated over
        if (!('ms' %in% disAggBy)) obs$ms<-'ALL_MATURITY'; #ms was aggregated over
        if (!('sc' %in% disAggBy)) obs$sc<-'ALL_SHELL';    #sc was aggregated over
        
        obs$type<-'observed';#add 'type' column
        
        #find factor combinations w/ non-zero abundance
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
        
        #aggregate comps as requested
        qry<-'select &&disAggByStr,sum(N) as N
              from mod
              group by &&disAggByStr
              order by &&disAggByStr';
        disAggByStr<-paste(paste(disAggBy,collapse=', '),'year, size',sep=', ')
        qry<-gsub('&&disAggByStr',disAggByStr,qry);
        mod<-sqldf::sqldf(qry);
        if (!('sx' %in% disAggBy)) mod$sx<-'ALL_SEX';      #sx was aggregated over
        if (!('ms' %in% disAggBy)) mod$ms<-'ALL_MATURITY'; #ms was aggregated over
        if (!('sc' %in% disAggBy)) mod$sc<-'ALL_SHELL';    #sc was aggregated over
        
        mod$type<-'estimated';#add 'type' column
                
        if (is.null(obs)){
            #find factor combinations w/ non-zero abundance
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
    
    #select factor combinations with non-zero abundance
    qry<-"select d.type,d.sx,d.ms,d.sc,d.year,d.size,d.N
          from dfr d, fcs f
          where d.sx=f.sx and d.ms=f.ms and d.sc=f.sc
          order by d.type,d.year,d.ms,d.sc,d.sx,d.size;"
    dfr<-sqldf::sqldf(qry);
    
    dfr$ms_sc<-paste(dfr$ms,dfr$sc,sep=', ')
    
    if (normalize){
        qry<-'select type, year&&normByStr, sum(N) as Nt
              from dfr
              group by type, year&&normByStr
              order by type, year&&normByStr;';
        normByStr<-'';
        if (!is.null(normBy)) normByStr<-paste(', ',normBy,sep='',collapse='');
        qry<-gsub('&&normByStr',normByStr,qry)
        tots<-sqldf::sqldf(qry);
#         cat("normalization totals:\n")
#         print(tots)
#         cat('\n\n')
        
        qry<-'select
                d.type,d.sx,d.ms,d.sc,d.ms_sc,d.year,d.size,d.N/t.Nt as N
              from dfr d, tots t
              where d.type=t.type &&joinByStr and d.year=t.year
              order by d.type,d.year,d.ms,d.sc,d.sx,d.size;';
        joinByStr<-'';
        if (!is.null(normBy)) joinByStr<-paste(' and d.',normBy,'=t.',normBy,sep='',collapse='');
        qry<-gsub('&&joinByStr',joinByStr,qry);
        dfr<-sqldf::sqldf(qry);        
    }

    #sum over years
    qry<-"select
            type,sx,ms,sc,size,sum(N)/count() as N
          from dfr
          group by type,sx,ms,sc,size
          order by type,sx,ms,sc,size";
    dfr<-sqldf::sqldf(qry);
    dfr$ms_sc<-paste(dfr$ms,dfr$sc,sep=', ')
    
    sxs<-unique(dfr$sx)
    ms.scs<-unique(dfr$ms_sc)
    uz<-unique(dfr$size);
    
    rng<-range(dfr$N,na.rm=TRUE,finite=TRUE);
    cat("rng = ",rng,'\n')
    
    ctr<-0;
    ps<-list();
#    for (sxp in sxs){ #loop over sex
#            dfrp<-dfr[(dfr$sx==sxp),]
            dfrp<-dfr;
            p <- ggplot(data=dfrp);
            p <- p + geom_bar(aes(x=size,y=N,fill=ms_sc),data=dfrp[dfrp$type=='observed',],stat="identity",position='identity',alpha=0.5)
            for (ms.scp in ms.scs){
                p <- p + geom_line(aes(x=size,y=N,colour=ms_sc),data=dfrp[(dfrp$type=='estimated')&(dfrp$ms_sc==ms.scp),],size=1)
            }
#             p <- p + scale_x_continuous(breaks=pretty(uz)) 
#             p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0))
            p <- p + ylim(0,rng[2])
            p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
            p <- p + labs(x="Size (mm)",y="proportion ")
            p <- p + facet_wrap(~sx,ncol=2) 
            p <- p + ggtitle(paste(label,': ',name,sep=''))
            p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
            p <- p + ggtheme
            if (showPlot) print(p);
            ctr<-ctr+1;
            ps[[ctr]]<-p
#    }
    return(ps)
}

#ps<-plotMeanSizeCompsGG(name,od,md,normalize=FALSE)
#ps<-plotMeanSizeCompsGG(name,od,md,disAggBy='sx',normalize=TRUE,normBy=NULL)
#ps<-plotMeanSizeCompsGG(name,od,md,disAggBy='sx',normalize=TRUE,normBy='sx')
# ps<-plotMeanSizeCompsGG(name,od,md,normalize=TRUE,normBy=c('sx','ms'))
# ps<-plotMeanSizeCompsGG(name,od,md,normalize=TRUE,normBy=c('sx','ms','sc'))
#ps<-plotMeanSizeCompsGG(name,NULL,md)
