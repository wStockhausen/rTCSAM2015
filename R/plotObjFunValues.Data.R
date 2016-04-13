#'
#'@title Plot data values (surveys and fisheries) used in the objective function, grouped by category.
#'
#'@description Function to plot data values (surveys and fisheries) used in the objective function.
#'
#'@param mdfr - melted dataframe of data values from call to getObjFunValues.getData(...)
#'@param variable - name of variable to plot
#'@param ggtheme - a ggplot2 theme
#'@param showPlot - flag to show plots
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return list of ggplot2 objects corresponding to different categories of priors.
#'
#'@import ggplot2
#'
#'@export
#'
plotObjFunValues.Data<-function(mdfr,
                                 variable='objfun',
                                 ggtheme=theme_grey(),
                                 showPlot=FALSE,
                                 verbose=FALSE){
    if (variable=='wgt'){
        fcn<-'average';
        ylab<-'likelihood weight';
    } else
    if (variable=='nll'){
        fcn<-'sum';
        ylab<-"negative log-likelihood";
    } else
    if (variable=='objfun'){
        fcn<-'sum';
        ylab<-'objective function value';
    }
    
    nms<-gsub('.','_',names(mdfr),fixed=TRUE);
    names(mdfr)<-nms;
    
    qry<-"select 
            model,source_name,catch_type,data_type,
            sex,maturity,shell_condition,
            &&fcn(value) as value
          from mdfr
          where 
            variable='&&variable'
          group by 
            model,source_name,catch_type,data_type,
            sex,maturity,shell_condition;"
    qry<-gsub('&&variable',variable,qry);
    qry<-gsub('&&fcn',fcn,qry)
    dfr<-sqldf::sqldf(qry);
    
    #shorten 'ALL's to ''s
    idx<-dfr$sex=='ALL_SEX';
    dfr$sex[idx]<-'all sexes';
    idx<-dfr$maturity=='ALL_MATURITY';
    dfr$maturity[idx]<-'all MSs';
    idx<-dfr$shell_condition=='ALL_SHELL_CONDITION';
    dfr$shell_condition[idx]<-'all SCs';
    idx<-dfr$shell_condition=='ALL_SHELL';
    dfr$shell_condition[idx]<-'all SCs';
    
    #change labels to lower case, change "_" to a space
    cols<-c('sex','maturity','shell_condition')
    for (col in cols) {
        dfr[[col]]<-tolower(dfr[[col]]);
        dfr[[col]]<-gsub('[_]',' ',dfr[[col]],fixed=FALSE);
    }
    
    #change "." or "_" to spaces
    cols<-c("source_name","catch_type");
    for (col in cols) dfr[[col]]<-gsub('[._]',' ',dfr[[col]],fixed=FALSE);
    
    dfr$fac<-paste(dfr$maturity,dfr$shell_condition,sep=', ')
    
    ums<-as.character(unique(dfr$model))
    n<-length(ums);#number of models
    if (verbose) cat("number of models =",n,'\n')
    
    ucts1<-as.character(unique(dfr$catch_type));
    ucts<-c("index catch","retained catch","discard catch","total catch");
    for (ct in ucts1){
        if (!(ct %in% ucts)) cat("Unrecognized catch type '",ct,"' in plotObjFunValues.Data()\n",sep='')
    }
    
    rng<-range(dfr$value)
    if (verbose) cat("range = [",paste(rng,collapse=', '),']\n',sep='')
    
    ps<-list();
    if (n==1){
        for (ct in ucts){
            dfrp<-dfr[dfr$catch_type==ct,];
            p <- ggplot(data=dfrp,aes(x=source_name,y=value,color=model,fill=fac,line=3))
            p <- p + geom_bar(stat="identity",position='dodge',alpha=1.0)
            p <- p + ylim(0,NA)
            p <- p + scale_fill_brewer(palette='Set1')
            p <- p + scale_color_brewer(palette='Dark2')
            p <- p + labs(x="Data Source",y=ylab);
            p <- p + guides(fill=guide_legend('Category',order=1));
            p <- p + guides(colour=guide_legend('Model',order=2));
            p <- p + ggtitle(paste("Data Components:",ct));
            p <- p + facet_grid(sex~data_type)
            p <- p + ggtheme;
            p<-p+theme(text = element_text(size=14), 
                       axis.text.x = element_text(angle=25, vjust=1.0, hjust=1));        
            if (showPlot) print(p);
            ps[[ct]]<-p;
        }
    } else {
        usrcs<-as.character(unique(dfr$source_name))
        for (src in usrcs){
            for (ct in ucts){
                dfrp<-dfr[(dfr$source_name==src)&(dfr$catch_type==ct),]
                if (nrow(dfrp)>0){
                    p <- ggplot(data=dfrp,aes(x=fac,y=value,fill=model,line=2))
                    p <- p + geom_bar(stat="identity",position='dodge',alpha=1.0)
 #                   p <- p + ylim(0,NA)
                    p <- p + scale_fill_brewer(palette='Set1')
                    p <- p + scale_color_brewer(palette='Dark2')
                    p <- p + labs(x="Data Source",y=ylab);
                    p <- p + guides(fill=guide_legend('Model',order=1));
                    p <- p + guides(colour=guide_legend('Category',order=2));
                    p <- p + ggtitle(paste(src,": ",ct,sep=''));
                    p <- p + facet_grid(sex~data_type)
                    p <- p + ggtheme;
                    p<-p+theme(text = element_text(size=14), 
                               axis.text.x = element_text(angle=25, vjust=1.0, hjust=1));        
                    if (showPlot) print(p);
                    ps[[paste(src,": ",ct,sep='')]]<-p;
                }#nrow(dfrp)>0
            }#cts (catch types)
        }#usrcs (data sources)
    }
#    if (showPlots) plotMulti.gg(plotlist=ps,cols=1)
    
    return(ps);
}

#ps<-plotObjFunValues.Data(mdfr.data.1,showPlots=TRUE)
#ps<-plotObjFunValues.Data(mdfr.data.2,showPlots=TRUE)
