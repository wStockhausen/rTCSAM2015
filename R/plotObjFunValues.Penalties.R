#'
#'@title Plot penalties used in the objective function, grouped by category.
#'
#'@description Function to plot penalties in the objective function.
#'
#'@param mdfr - melted dataframe of penalties from call to getObjFunValues.Penalties(...)
#'@param variable - name of variable to plot
#'@param ggtheme - a ggplot2 theme
#'@param showPlot - flag to show plots immediately
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return ggplot2 object
#'
#'@import ggplot2
#'
#'@export
#'
plotObjFunValues.Penalties<-function(mdfr,
                                     variable='objfun',
                                     ggtheme=theme_grey(),
                                     showPlot=FALSE,
                                     verbose=FALSE){
    if (variable=='wgt'){
        ylab<-'likelihood weight';
    } else
    if (variable=='nll'){
        ylab<-"negative log-likelihood";
    } else
    if (variable=='objfun'){
        ylab<-'objective function value';
    }
    qry<-"select * from mdfr
          where variable='&&variable';"
    qry<-gsub('&&variable',variable,qry);
    dfr<-sqldf::sqldf(qry);
    
    rng<-range(dfr$value,na.rm=TRUE,finite=TRUE);
    ucats<-unique(dfr$category);
#     for (ucat in ucats){
#         p <- ggplot(data=dfr[dfr$category==ucat])
        p <- ggplot(data=dfr[])
        p <- p + geom_bar(aes(x=paste(name,'[',sprintf('%02d',level),']',sep=''),y=value,fill=model),stat="identity",position='dodge',alpha=1.0)
        p <- p + scale_y_continuous(breaks=pretty(rng),expand=c(0.01,0))
        p <- p + labs(x="Penalty",y=ylab)
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
        p <- p + ggtitle('Penalties')
        p <- p + facet_wrap(~category,scales="free_x")
        p <- p + ggtheme
        p<-p+theme(text = element_text(size=14), 
                   axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
        if (showPlot) print(p);
#    }
    return(p);
}

#ps<-plotObjFunValues.Penalties(mdfr.pens.1,showPlots=TRUE)
#ps<-plotObjFunValues.Penalties(mdfr.pens.2,showPlots=TRUE)
