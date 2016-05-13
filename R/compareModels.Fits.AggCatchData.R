#'
#'@title Compare fits to aggregated catch data (abundance or biomass) for TCSAM2015 model runs
#'
#'@description Function to compare fits to aggregated catch data (abundance or biomass) for TCSAM2015 model runs.
#'
#'@param mdfr - dataframe from call to \code{getMDFR.FitsForFleets(...)}
#'@param fleets - fleets to plot [NULL for all]
#'@param catch.types - catch types ('index.catch', retained.catch', etc) to plot [NULL for all]
#'@param logscale - flag (T/F) to plot on log scale
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param title - plot title
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param ggtheme - graphical theme for plot
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@details Uses \code{reshape2::dcast(...)}
#'
#'@import ggplot2
#'
#'@export
#'
compareModels.Fits.AggCatchData<-function(mdfr,
                                          fleets=NULL,
                                          catch.types=NULL,
                                          logscale=FALSE,
                                          xlab="Year",
                                          ylab="",
                                          title="",
                                          xlim=NULL,
                                          ylim=NULL,
                                          ggtheme=ggplot2::theme_grey(),
                                          showPlot=TRUE,
                                          pdf=NULL,
                                          width=8,
                                          height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    if (is.null(fleets)) fleets<-sort(unique(mdfr$fleet));#fleet names
    if (is.null(catch.types)) catch.types<-c("index catch",
                                             "total catch",
                                             "retained catch",
                                             "discard catch");
    
    mdfr1<-mdfr[(mdfr$fleet %in% fleets)&(mdfr$catch.type %in% catch.types),];
    
    plots<-list();
    for (fleet in fleets){
        if (verbose) cat("Plotting fleet '",fleet,"'.\n",sep='');
        mdfrp<-mdfr1[mdfr1$fleet==fleet,];#select results for fleet        
        ddfr<-reshape2::dcast(mdfrp,model+catch.type+y+x+m+s~var,fun.aggregate=sum,na.rm=FALSE,value.var='val',drop=TRUE)
        # if (logscale) {
        #     ddfr$obs<-log10(ddfr$obs);
        #     ddfr$est<-log10(ddfr$est);
        #     ddfr$uci<-log10(ddfr$uci);
        #     ddfr$lci<-log10(ddfr$lci);
        # }
        
        #restructure ddfr by "observed" and "estimated"
        iobs<-which(names(ddfr) %in% c('obs','uci','lci'));
        ddfrm<-ddfr[,-iobs];
        nms<-names(ddfrm);
        nms[length(nms)]<-"val";
        names(ddfrm)<-nms;
        ddfrm$lci<-NA;
        ddfrm$uci<-NA;
        ddfrm$type<-"estimated";
        
        iest<-which(names(ddfr)=='est');
        ddfro<-ddfr[,-iest];
        names(ddfro)<-c(nms,"lci","uci");
        ddfro$type<-"observed";
        
        ddfr<-rbind(ddfro,ddfrm);
        ddfr<-ddfro;
        
        if (is.null(ylim)) ylim<-c(0,1.2*max(ddfr$val,na.rm=TRUE));
        if (logscale) ylim<-c(NA,ylim[2]);
        
        #do plot
        pd<-position_identity();
        p <- ggplot(aes_string(x='y',y='val',colour='model',fill='type',shape='model',linetype='type'),
                    data=ddfr)
        p <- p + scale_linetype_manual(values=c(observed=3,estimated=1))
        #p <- p + scale_shape_manual(values=c(observed=19,estimated=1))
        p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1)
        p <- p + geom_point(position=pd,size=2)
        p <- p + geom_line( position=pd,size=1,alpha=0.8)
        if (!is.null(xlim))  p <- p + scale_x_continuous(limits=xlim);
        p <- p + scale_y_continuous(limits=ylim,trans=ifelse(logscale,"log10","identity"),oob=scales::squish);
        if (!is.null(xlab)) p <- p + xlab(xlab)
        if (!is.null(ylab)) p <- p + ylab(ylab)
    #    if (!is.null(ylim)) p <- p + ylim(ylim)
        p <- p + facet_grid(x+m+s+catch.type~.)
        p <- p + guides(linetype=guide_legend('type',order=1),
                        shape   =guide_legend('type',order=1),
                        fill    =guide_legend('type',order=1),
                        colour  =guide_legend('type',order=1))
        p <- p + ggtitle(title);
        p <- p + ggtheme;
        print(p)
        plots[[fleet]]<-p;
    }#fleets
    
    return(invisible(plots))
}
