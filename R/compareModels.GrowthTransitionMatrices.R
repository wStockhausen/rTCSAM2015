#'
#'@title Compare growth transition matrices for TCSAM2015 model runs
#'
#'@description Function to compare growth transition matrices for TCSAM2015 model runs.
#'
#'@param mdfr - dataframe from call to \code{getMDFR.GrowthTransitionMatrices(...)} for n.at.z data
#'@param nrow - number of rows per page for output plots
#'@param ncol - number of columns per page for output plots
#'@param ggtheme - graphical theme for plot
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print diagnostic output
#'
#'@return recursive list of lists of ggplot2 objects
#'
#'@details Plots are produced by sex and faceted by parameter combination.
#'
#'@import ggplot2
#'
#'@export
#'
compareModels.GrowthTransitionMatrices<-function(mdfr,
                                                  nrow=4,
                                                  ncol=3,
                                                  xlab="post-molt size (mm CW)",
                                                  ylab="proportion",
                                                  xlim=NULL,
                                                  ylim=NULL,
                                                  ggtheme=ggplot2::theme_grey(),
                                                  showPlot=TRUE,
                                                  pdf=NULL,
                                                  width=8,
                                                  height=6,
                                                  verbose=FALSE){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    xs<-c("male","female");
    pcs<-sort(unique(mdfr$pc))
    zs<-sort(unique(mdfr$z));
    
    mxp<-nrow*ncol;
    
    plots<-list();
    for (x in xs){
        idx<-mdfr$x==x;
        pps<-list();
        for (pc in pcs){
            idp<-mdfr$pc==pc;
            if (verbose) cat("Checking",x,pc,"\n");
            pgs<-list();
            mdfrp<-mdfr[idx&idp,];#select results for sex and pc
            
            npg<-ceiling(length(zs)/mxp)
            
            rng<-range(mdfrp$val,na.rm=TRUE,finite=TRUE);
            cat("rng = ",rng,'\n')
            
            for (pg in 1:npg){ #loop over pages
                dfrp<-mdfrp[(mdfrp$z %in% zs[(pg-1)*mxp+1:mxp]),]
                #do plot
                pd<-position_identity();
                p <- ggplot(data=dfrp,mapping=aes(x=zp,y=val,colour=model))
                p <- p + geom_line(size=1)
    #             p <- p + scale_x_continuous(breaks=pretty(uz)) 
    #             p <- p + scale_y_continuous(breaks=pretty(rng),expand=c(0.01,0))
                p <- p + ylim(0,rng[2])
                p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
                p <- p + labs(x=xlab,y=ylab)
                p <- p + facet_wrap(~z,ncol=ncol) 
                p <- p + ggtitle(paste('pc:',pc,': ',x,sep=''))
                p <- p + guides(colour=guide_legend('model'))
                p <- p + ggtheme
                if (showPlot) print(p);
                pgs[[pg]]<-p;
            }#pg
            pps[[pc]]<-pgs;
        }#ss
        pxs[[x]]<-pms;
    }#xs

    return(invisible(plots))
}
