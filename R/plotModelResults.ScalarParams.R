#'
#'@title Function to plot parameter values and associated uncertainty from different models.
#'
#'@description This function plots parameters values, together with their limits
#'(if any) and the posterior distributions implied by their estimated standard,
#'errors from several models.
#'
#'@param dfr - dataframe from call to extractModelResults.Params
#'@param vfr - dataframe from call to extractModelResults.StdDevs
#'@param nc - number of columns of plots per page
#'@param nr - number of rows of plots per page
#'@param showPlot - flag to show plots
#'@param pdf - file name for printing plots to a pdf file (or NULL to print to screen)
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - list with plots as elements
#'
#'@import ggplot2
#'
#'@export
#'
plotModelResults.ScalarParams<-function(dfr,
                                        vfr=NULL,
                                        nc=3,nr=4,
                                        showPlot=TRUE,
                                        pdf=NULL,
                                        verbose=FALSE){
    if (showPlot&!is.null(pdf)){
        pdf(file=pdf,width=8.5,height=11,onefile=TRUE);
        on.exit(dev.off());
    }
    ups<-sort(unique(dfr$param));  #unique parameters
    np<-length(ups);               #number of unique parameters
    npg<-ceiling(np/(nc*nr));      #number of pages
    plots<-list();
    for (pg in 1:npg){
        upsp <- ups[1:(nc*nr)+(pg-1)*nc*nr];
        idx<- dfr$param %in% upsp
        dfrsp<-dfr[idx,];
        if (!is.null(vfr)){
           idx<- vfr$param %in% upsp
           vfrsp<-vfr[idx,];
        }
       p <- ggplot(data=dfrsp)
       p <- p + geom_rect(mapping=aes_string(xmin='min',xmax='max'),ymin=I(0),ymax=I(0.7),alpha=0.5,fill='grey')
       p <- p + geom_vline(aes_string(xintercept='init',colour='case'),linetype=2,alpha=0.7,size=1);
       p <- p + geom_vline(aes_string(xintercept='value',colour='case'),linetype=1,size=1);
##       p <- p + geom_point(aes_string(x='value',colour='scl'),size=4,y=I(0.5))
       p <- p + guides(colour=guide_legend())
       p <- p + scale_y_continuous(breaks=NULL)
       p <- p + labs(x='parameter value',y='')
       if (!is.null(vfr)&&(nrow(vfrsp)>0)){
           p <- p + geom_area(aes(x=x,y=y,fill=case),data=vfrsp,alpha=0.3,position="identity")
           p <- p + guides(fill=guide_legend())
       }
       p <- p + facet_wrap(~param,ncol=nc,nrow=nr,drop=FALSE,scales="free_x");
       if (showPlot) print(p);
       plots[[pg]]<-p;
    }
    return(invisible(plots));
}

