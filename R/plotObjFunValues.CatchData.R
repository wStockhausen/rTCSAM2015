#'
#'@title Plot prior values used in the objective function, grouped by category.
#'
#'@description Function to plot prior values in the objective function.
#'
#'@param mdfr - melted dataframe of prior values from call to getObjFunValues.Priors(...)
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
plotObjFunValues.Priors<-function(mdfr,
                                  variable='objfun',
                                  ggtheme=theme_grey(),
                                  showPlots=FALSE){
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
    ps<-list();
    for (ucat in ucats){
        dfrp<-dfr[dfr$category==ucat,];
        p <- ggplot(data=dfrp)
        p <- p + geom_bar(aes(x=paste(name,'[',sprintf('%02d',level),']',sep=''),y=value,fill=model),stat="identity",position='dodge',alpha=1.0)
        p <- p + scale_y_continuous(breaks=pretty(rng),limits=rng,expand=c(0.01,0))
        p <- p + labs(x="Parameter",y=ylab)
        p <- p + guides(fill=guide_legend(''),colour=guide_legend(''))
        p <- p + ggtitle(paste('priors: ',ucat,sep=''))
        p <- p + ggtheme
        p<-p+theme(text = element_text(size=14), 
                   axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
        if (showPlots) print(p);
        ps[[ucat]]<-p;
    }
    return(ps);
}

#ps<-plotObjFunValues.Priors(mdfr.priors,showPlots=TRUE)