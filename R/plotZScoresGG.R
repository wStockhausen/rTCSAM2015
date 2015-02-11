#'
#'@title Plot z-scores from fits to data
#'
#'@description Function to plot z-scores from fits to data using ggplot2.
#'
#'@param afits - 
#'@param xlim - x axis limits (computed internally if NULL)
#'@param ylim - y axis limits (computed internally if NULL)
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param label - title for graph page
#'@param ggtheme - theme for ggplot2
#'@param showPlot - flag to print plot immediately
#'
#'@return ggplot2 object
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
                        showPlot=TRUE){
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
                lci<-qnorm(ci[1],mean=obs,sd=sdv);
                uci<-qnorm(ci[2],mean=obs,sd=sdv);
            } else if (tolower(pdfType)=='lognormal'){
                #lognormal, sdv on ln-scale
                lci<-qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
                uci<-qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
            } else if (tolower(pdfType)=='norm2'){
                #normal, sdv on arithmetic scale
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
            dfrp<-data.frame(sex=afit$sx,maturity=afit$ms,`shell condition`=afit$sc,
                             year=as.numeric(names(obs)),val=obs,lci=lci,uci=uci);
            odfr<-rbind(odfr,dfrp)
        }
    }
    nms<-names(odfr);
    nms[3]<-'shell condition';
    names(odfr)<-nms;
    
    mdfr<-NULL;
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            dfrp<-data.frame(sex=afit$sx,maturity=afit$ms,sc=afit$sc,
                             year=as.numeric(names(nll$mod)),val=nll$mod,zscr=nll$zscrs);
            mdfr<-rbind(mdfr,dfrp);
        }
    }
    nms<-names(mdfr);
    nms[3]<-'shell condition';
    names(mdfr)<-nms;
        
    xlim<-range(odfr$year);
    pd<-position_dodge(0.2)
    
    p1 <- ggplot(aes_string(x='year',y='val',colour='maturity',shape='`shell condition`',fill='maturity'),data=odfr)
    p1 <- p1 + geom_errorbar(aes_string(ymin='lci',ymax='uci'),width=1,position=pd)
    p1 <- p1 + geom_point(position=pd,size=3)
    p1 <- p1 + geom_line(position=pd,size=1,linetype=3,alpha=0.5)
    p1 <- p1 + geom_line(data=mdfr,position=pd,size=1,linetype=1,alpha=1.0)
    p1 <- p1 + xlim(xlim);
    if (!is.null(ylab)) p1 <- p1 + ylab(ylab)
#    if (!is.null(ylim)) p1 <- p1 + ylim(ylim)
    p1 <- p1 + facet_wrap(~sex,nrow=1);
    p1 <- p1 + guides(colour=guide_legend('maturity',order=1),fill=guide_legend('maturity',order=1),shape=guide_legend('shell condition',order=2))
    p1 <- p1 + ggtitle(label);
    p1 <- p1 + ggtheme;
    
    odfr$val<-log(odfr$val);
    odfr$uci<-log(odfr$uci);
    odfr$lci<-log(odfr$lci);
    mdfr$val<-log(mdfr$val);
    
    p2 <- ggplot(aes_string(x='year',y='val',colour='maturity',shape='`shell condition`',fill='maturity'),data=odfr)
    p2 <- p2 + geom_errorbar(aes_string(ymin='lci',ymax='uci'),width=1,position=pd)
    p2 <- p2 + geom_point(position=pd,size=3)
    p2 <- p2 + geom_line(position=pd,size=1,linetype=3,alpha=0.5)
    p2 <- p2 + geom_line(data=mdfr,position=pd,size=1,linetype=1,alpha=1)
    p2 <- p2 + xlim(xlim);
    if (!is.null(ylab)) p2 <- p2 + ylab(paste(ylab,'[ln-scale]'))
#    if (!is.null(ylim)) p2 <- p2 + ylim(ylim)
    p2 <- p2 + facet_wrap(~sex,nrow=1)
    p2 <- p2 + guides(colour=guide_legend('maturity',order=1),fill=guide_legend('maturity',order=1),shape=guide_legend('shell condition',order=2))
    p2 <- p2 + ggtheme;
    
    ylim<-max(abs(mdfr$zscr),na.rm=TRUE)*c(-1,1);
    p3 <- ggplot(aes_string(x='year',y='zscr',colour='maturity',shape='`shell condition`',fill='maturity'),data=mdfr)
    p3 <- p3 + geom_point(position=pd,size=3)
    p3 <- p3 + xlim(xlim);
    p3 <- p3 + ylim(ylim);
    p3 <- p3 + ylab('z-scores')
    p3 <- p3 + facet_wrap(~sex,nrow=1)
    p3 <- p3 + guides(colour=guide_legend('maturity',order=1),fill=guide_legend('maturity',order=1),shape=guide_legend('shell condition',order=2))
    p3 <- p3 + ggtheme;

    if (showPlot) plotMulti.GG(p1,p2,p3,cols=1);
    
    return(list(arscale=p1,lnscale=p2,zscores=p3));
}
