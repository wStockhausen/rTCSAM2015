#'
#'@title Plot data values (surveys and fisheries) used in the objective function, grouped by category.
#'
#'@description Function to plot data values (surveys and fisheries) used in the objective function.
#'
#'@param mdfr - melted dataframe of data values from call to getObjFunValues.getData(...)
#'@param variable - name of variable to plot
#'@param ggtheme - a ggplot2 theme
#'@param showPlots - flag to show plots
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
                                 showPlots=FALSE){
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
    dfr$sex[idx]<-'';
    idx<-dfr$maturity=='ALL_MATURITY';
    dfr$maturity[idx]<-'';
    idx<-dfr$shell_condition=='ALL_SHELL_CONDITION';
    dfr$shell_condition[idx]<-'';
    idx<-dfr$shell_condition=='ALL_SHELL';
    dfr$shell_condition[idx]<-'';
    
    dfr$fac<-paste(dfr$sex,dfr$maturity,dfr$shell_condition,sep=' ')
    
    n<-length(unique(dfr$model));#number of models
    cat("number of models =",n,'\n')
    
    cts<-unique(dfr$catch_type);
    
    rng<-range(dfr$value)
    cat("range = [",paste(rng,collapse=', '),']\n',sep='')
    
    ps<-list();
    for (ct in cts){
        p <- ggplot(data=dfr[dfr$catch_type==ct,],aes(x=source_name,y=value,fill=fac,colour='model',line=2))
        p <- p + geom_bar(stat="identity",position='dodge',alpha=1.0)
        p <- p + ylim(0,rng[2])
        p <- p + scale_fill_brewer(palette='Set1')
        p <- p + scale_color_brewer(palette='Dark2')
        p <- p + labs(x="Data Source",y=ylab);
        p <- p + guides(fill=guide_legend('Category',order=1));
        p <- p + guides(colour=guide_legend('Model',order=2));
        p <- p + ggtitle(paste("Data Components:",ct));
        p <- p + facet_wrap(~data_type)
        p <- p + ggtheme;
        p<-p+theme(text = element_text(size=14), 
                   axis.text.x = element_text(angle=25, vjust=1.0, hjust=1));        
        if (showPlots) print(p);
        ps[[ct]]<-p;
    }
#    if (showPlots) plotMulti.gg(plotlist=ps,cols=1)
    
    return(ps);
}

#ps<-plotObjFunValues.Data(mdfr.data.1,showPlots=TRUE)
#ps<-plotObjFunValues.Data(mdfr.data.2,showPlots=TRUE)
