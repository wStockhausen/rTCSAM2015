#'
#'@title Plot z-scores from fits to data
#'
#'@description Function to plot z-scores from fits to data using ggplot2.
#'
#'@param afits - 
#'@param ci - confidence interval to plot
#'@param xlim - x axis limits (computed internally if NULL)
#'@param ylim - y axis limits (computed internally if NULL)
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param label - title for graph page
#'@param ggtheme - theme for ggplot2
#'@param showPlot - flag to print plot immediately
#'
#'@return list of ggplot2 objects
#'
#'@import ggplot2
#'@import reshape2
#'
#'@export
#'
plotZScoresGG<-function(afits,
                        ci=0.95,
                        xlim=NULL,
                        ylim=NULL,
                        xlab="years",
                        ylab="z-scores",
                        label="",
                        ggtheme=theme_grey(),
                        showPlot=FALSE){
    cat("---Running plotZScoresGG(...) for",label,"\n");
    
    label<-gsub("[_]"," ",label);#replace "_"'s with blank spaces
    
    nf<-length(afits);
    
    ci<-c((1-ci)/2,1-(1-ci)/2);
    
    odfr<-NULL;
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            pdfType<-nll$nll.type;
            obs<-nll$obs;
            sdv<-nll$stdv;
            if (tolower(pdfType)=='normal'){
                #normal, sdv on arithmetic scale
                cat('using err type = normal\n')
                lci<-qnorm(ci[1],mean=obs,sd=sdv);
                uci<-qnorm(ci[2],mean=obs,sd=sdv);
            } else if (tolower(pdfType)=='lognormal'){
                #lognormal, sdv on ln-scale
                cat('using err type = lognormal\n')
                lci<-qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
                uci<-qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
            } else if (tolower(pdfType)=='norm2'){
                #normal, sdv on arithmetic scale
                cat('using err type = normal, but fit uses norm2\n')
                lci<-qnorm(ci[1],mean=obs,sd=1);
                uci<-qnorm(ci[2],mean=obs,sd=1);
            } else {
                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                cat('Error in plotZScoresGG.\n')
                cat("pdfType '",pdfType,"' not recognized!!\n")
                cat("Exiting function.\n")
                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                return(NULL)
            }
            dfrp<-data.frame(type='observed',x=afit$x,m=afit$m,s=afit$s,
                             year=as.numeric(names(obs)),val=obs,lci=lci,uci=uci,zscr=NA);
            odfr<-rbind(odfr,dfrp)
        }
    }
    
    mdfr<-NULL;
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            dfrp<-data.frame(type='estimated',x=afit$x,m=afit$m,s=afit$s,
                             year=as.numeric(names(nll$mod)),val=nll$mod,lci=NA,uci=NA,zscr=nll$zscrs);
            mdfr<-rbind(mdfr,dfrp);
        }
    }
        
    xlim<-range(odfr$year);
    
    cdfr<-rbind(odfr,mdfr);
    cdfr$x<-tolower(gsub("_"," ",cdfr$x));
    cdfr$m<-tolower(gsub("_"," ",cdfr$m));
    cdfr$s<-tolower(gsub("_"," ",cdfr$s));
    cdfr$class<-paste(cdfr$m,cdfr$s,sep=", ")
    
    cdfr<-removeImmOS(cdfr);
    
    cdfr<-cdfr[(xlim[1]<=cdfr$year)&(cdfr$year<=xlim[2]),]
    
    label<-gsub("_"," ",label);

    #arithmetic-scale fits
    pd<-position_identity(0.0)
    p <- ggplot(aes_string(x='year',y='val',colour='type',fill='type',shape='type',linetype='type'),data=cdfr)
    p <- p + scale_linetype_manual(values=c(observed=3,estimated=1))
    p <- p + scale_shape_manual(values=c(observed=19,estimated=1))
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1)
    p <- p + geom_point(position=pd,size=3)
    p <- p + geom_line( position=pd,size=1,alpha=1)
    p <- p + xlim(xlim);
    if (!is.null(ylab)) p <- p + ylab(ylab)
#    if (!is.null(ylim)) p <- p + ylim(ylim)
    p <- p + facet_grid(class~x)
    p <- p + guides(linetype=guide_legend('type',order=1),
                    shape   =guide_legend('type',order=1),
                    fill    =guide_legend('type',order=1),
                    colour  =guide_legend('type',order=1))
    p <- p + ggtitle(label);
    p <- p + ggtheme;

    p1 <- p;#save plot with current dataframe
    
    #ln-scale fits
    cdfr$val<-log(cdfr$val);
    cdfr$uci<-log(cdfr$uci);
    cdfr$lci<-log(cdfr$lci);
    p2 <- p %+% cdfr;#change to updated dataframe
        
    #zscores
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    p3 <- ggplot(aes_string(x='year',y='zscr',colour='s',shape='s',fill='s'),data=cdfr)
    p3 <- p3 + geom_point(position=pd,size=3,alpha=0.8)
    p3 <- p3 + xlim(xlim);
    p3 <- p3 + ylim(ylim);
    p3 <- p3 + ylab('z-scores')
    p3 <- p3 + facet_grid(m~x)
    p3 <- p3 + guides(colour=guide_legend('shell condition',order=1),
                      fill  =guide_legend('shell condition',order=1),
                      shape =guide_legend('shell condition',order=1))
    p3 <- p3 + ggtitle(label);
    p3 <- p3 + ggtheme;

#    if (showPlot) plotMulti.GG(p1,p2,p3,cols=1);
    if (showPlot) {print(p1); print(p2); print(p3);}
    
    cat("---Done running plotZScoresGG(...)\n\n");
    return(invisible(list(arscale=p1,lnscale=p2,zscores=p3)));
}
