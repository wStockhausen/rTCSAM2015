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
#'@details Plots are produced by sex and parameter combination, 
#'and faceted by pre-molt size ('z' column). nrow*ncol determines
#'the maximum number of plots per figure.
#'
#'@import ggplot2
#'
#'@export
#'
compareModels.GrowthTransitionMatrices<-function(mdfr,
                                                  nrow=7,
                                                  ncol=5,
                                                  xlab="post-molt size (mm CW)",
                                                  ylab="proportion",
                                                  xlim=NULL,
                                                  ylim=NULL,
                                                  ggtheme=ggplot2::theme_grey(),
                                                  showPlot=FALSE,
                                                  pdf=NULL,
                                                  width=8,
                                                  height=10,
                                                  verbose=FALSE){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    uxs<-c("male","female");
    upc<-as.character(sort(unique(mdfr$pc)));
    uzs<-as.numeric(sort(unique(as.numeric(mdfr$z))));
    if (verbose) {
        cat("uxs =",uxs,"\n");
        cat("upc =",upc,"\n");
        cat("uzs =",uzs,"\n");
    }
    mdfr$z<-as.character(mdfr$z);
    
    mxp<-nrow*ncol;
    if (verbose) cat("\tmxp =",mxp,"\n");
    
    pxs<-list();
    for (x in uxs){
        if (verbose) cat("Checking x =",x,"\n");
        idx<-(mdfr$x==x);
        if (verbose) cat("sum(idx) =",sum(idx),"\n");
        pcs<-list();
        for (pc in upc){
            if (verbose) cat("\tChecking pc =",pc,"\n");
            idp<-(mdfr$pc==pc);
            mdfrp<-mdfr[idx&idp,];#select results for sex and pc
            if (verbose) cat("\tnrow(mdfrp) =",nrow(mdfrp),"\n");
            npg<-ceiling(length(uzs)/mxp)
            
            rng<-range(mdfrp$val,na.rm=TRUE,finite=TRUE);
            cat("\t\trng = ",rng,'\n')
            
            pgs<-list();
            for (pg in 1:npg){ #loop over pages
                if (verbose) cat("\t\tcreating pg =",pg,"\n");
                dfrp<-mdfrp[(mdfrp$z %in% uzs[(pg-1)*mxp+1:mxp]),];
                dfrp$z<-factor(as.character(dfrp$z),as.character(uzs));
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
                p <- p + ggtitle(paste0(pc,': ',x))
                p <- p + guides(colour=guide_legend('model'))
                p <- p + ggtheme
                if (showPlot) print(p);
                pgs[[pg]]<-p;
            }#pg
            pcs[[pc]]<-pgs;
        }#pc
        pxs[[x]]<-pcs;
    }#x

    return(invisible(pxs))
}
