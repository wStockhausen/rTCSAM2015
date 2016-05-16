#'
#'@title Compare z-scores from fits to size compositions from catch data for TCSAM2015 model runs
#'
#'@description Function to compare z-scores from fits to size compositions from catch data for TCSAM2015 model runs.
#'
#'@param mdfr - dataframe from call to \code{getMDFR.ZScoresForFleets(...)} for n.at.z data
#'@param fleets - fleets to plot [NULL for all]
#'@param catch.types - catch types ('index.catch', retained.catch', etc) to plot [NULL for all]
#'@param variable - variable to plot ("pearsons" or "nlls")
#'@param nrow - number of rows per page for output plots
#'@param ncol - number of columns per page for output plots
#'@param xlab - x axis label
#'@param ylab - y axis label
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param ggtheme - graphical theme for plot
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print diagnostic output
#'
#'@return recursive list of lists  of ggplot2 objects, named by fleet, catch type, sex, maturity state, and shell condition
#'
#'@details Plots are produced by fleet, catch type, sex, maturity state, and shell condition and faceted by year.
#'
#'@import ggplot2
#'
#'@export
#'
compareModels.ZScores.NatZData<-function(mdfr,
                                         fleets=NULL,
                                         catch.types=NULL,
                                         variable="pearsons",
                                         nrow=4,
                                         ncol=3,
                                         xlab="Size (mm CW)",
                                         ylab=variable,
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
    
    if (is.null(fleets)) fleets<-sort(unique(mdfr$fleet));#fleet names
    if (is.null(catch.types)) catch.types<-c("index catch",
                                             "total catch",
                                             "retained catch",
                                             "discard catch");
    xs<-c("male","female","all sex");
    ms<-c("immature","mature","all maturity");
    ss<-c("new shell","old shell","all shell");
    zs<-sort(unique(mdfr$z));
    
    mxp<-nrow*ncol;
    
    mdfr1<-mdfr[(mdfr$fleet %in% fleets)&(mdfr$catch.type %in% catch.types)&(mdfr$var==variable),];
    
    plots<-list();
    for (fleet in fleets){
        if (verbose) cat("Plotting fleet '",fleet,"'.\n",sep='');
        idf<-mdfr1$fleet==fleet;
        pcs<-list();
        for (catch.type in catch.types){
            if (verbose) cat("Plotting catch type '",catch.type,"'.\n",sep='');
            idc<-mdfr1$catch.type==catch.type;
            pxs<-list();
            for (x in xs){
                idx<-mdfr1$x==x;
                pms<-list();
                for (m in ms){
                    idm<-mdfr1$m==m;
                    pss<-list();
                    for (s in ss){
                        ids<-mdfr1$s==s;
                        if (verbose) cat("Checking",x,m,s,"\n");
                        pgs<-list();
                        if (sum(idf&idc&idx&idm&ids)==0){
                            if (verbose) cat("--Dropping",x,m,s,"\n");
                        } else {
                            if (verbose) cat("--Plotting",x,m,s,"size comps\n");
                            mdfrp<-mdfr1[idf&idc&idx&idm&ids,];#select results for fleet, catch type, and sex
                            
                            ys<-sort(unique(mdfrp$y));
                            npg<-ceiling(length(ys)/mxp)
                            
                            rng<-max(abs(mdfrp$val),na.rm=TRUE,finite=TRUE);
                            cat("rng = ",rng,'\n')
                            
                            for (pg in 1:npg){ #loop over pages
                                dfrp<-mdfrp[(mdfrp$y %in% ys[(pg-1)*mxp+1:mxp]),]
                                #do plot
                                pd<-position_identity();
                                p <- ggplot(data=dfrp,aes(x=z,y=val,colour=model))
                                p <- p + geom_line(size=1)
                    #             p <- p + scale_x_continuous(breaks=pretty(uz)) 
                    #             p <- p + scale_y_continuous(breaks=pretty(rng),expand=c(0.01,0))
                                p <- p + ylim(-rng,rng)
                                p <- p + geom_hline(yintercept=0,colour='black',size=0.5)
                                p <- p + labs(x=xlab,y=ylab)
                                p <- p + facet_wrap(~y,ncol=ncol) 
                                p <- p + ggtitle(paste(fleet,': ',x,", ",m,", ",s,': ',catch.type,sep=''))
                                p <- p + guides(fill=guide_legend('observed'),colour=guide_legend('estimated'))
                                p <- p + ggtheme
                                if (showPlot) print(p);
                                pgs[[pg]]<-p;
                            }#pg
                        }#if
                        pss[[s]]<-pgs;
                    }#ss
                    pms[[m]]<-pss;
                }#ms
                pxs[[x]]<-pms;
            }#xs
            pcs[[catch.type]]<-pxs;
        }#catch.types
        plots[[fleet]]<-pcs;
    }#fleets
    
    return(invisible(plots))
}
