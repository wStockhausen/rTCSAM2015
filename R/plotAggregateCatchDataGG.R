#'
#'@title Plot aggregate catch data
#'
#'@description Plot aggregate (numbers or biomass) catch data from fisheries or surveys
#'
#'@param label - fishery or survey name
#'@param obs - list of observed data 
#'@param mod - list of model-predicted data
#'@param ci - confidence interval
#'@param pdfType - pdf type for ci calculations
#'@param logscale - flag to plot on log scale
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param ggtheme - ggplot2 theme
#'@param showPlot - flag to print plot immediately
#'
#'@return ggplot2 object
#'
#'@import ggplot2
#'
#'@export
#'
plotAggregateCatchDataGG<-function(label=NULL,
                                   obs=NULL,
                                   mod=NULL,
                                   ci=0.95,
                                   pdfType='normal',
                                   logscale=FALSE,
                                   xlab='year',
                                   ylab='',
                                   xlim=NULL,
                                   ylim=NULL,
                                   ggtheme=theme_grey(),
                                   showPlot=FALSE){
    cat("---Running plotAggregateCatchDataGG(...) for",label,"with",ylab,"\n");
    units<-gsub("_"," ",tolower(mod$units));
    ylab<-paste(ylab," (",units,")",sep='');
    
    if (logscale){ylab<-paste(ylab," [ln-scale]");}
    
    ci<-c((1-ci)/2,1-(1-ci)/2);
    
    #observations
    odfr<-NULL;
    if (!is.null(obs)){
        odfr<-reshape2::melt(obs$data,value.name='val');
        cvs <-reshape2::melt(obs$cvs,value.name='cv');
        odfr<-cbind(odfr,cv=cvs$cv);
        if (tolower(pdfType)=='normal'){
            #normal
            cat('Using normal error structure for error bars\n')
            sd<-odfr$cv*odfr$val;#sd on arithmetic scale
            odfr$sd<-sd;
            odfr$lci<-qnorm(ci[1],mean=odfr$val,sd=sd);
            odfr$uci<-qnorm(ci[2],mean=odfr$val,sd=sd);
        } else if (tolower(pdfType)=='lognormal'){
            #lognormal
            cat('Using lognormal error structure for error bars\n')
            sd<-sqrt(log(1+odfr$cv^2));#sd on ln-scale
            odfr$sd<-odfr$cv*odfr$val; #sd on arithmetic scale
            odfr$lci<-exp(qnorm(ci[1],mean=log(odfr$val),sd=sd));
            odfr$uci<-exp(qnorm(ci[2],mean=log(odfr$val),sd=sd));
        } else {
            cat('No error bars\n')
            odfr$lci<-NA;
            odfr$uci<-NA;
        }
        odfr$facs<-paste(odfr$x,odfr$m,odfr$s)
        #find factor combinations which are NOT all 0's
        tots<-reshape2::dcast(odfr,x+m+s~.,value.var='val',fun.aggregate=sum,drop=TRUE)
        tots<-tots[!(tots$`.`==0),]
        facs<-paste(tots$x,tots$m,tots$s)
        odfr<-odfr[odfr$facs %in% facs,]
        cat("data factors =",paste("'",facs,"'",sep="",collapse=", "),"\n")
#         cat("nrow(odfr) = ",nrow(odfr),'\n')
#         cat("is.null(odfr$uci) = ",is.null(odfr$uci),'\n')
#         cat("is.null(odfr$lci) = ",is.null(odfr$lci),'\n')
        if (logscale){
            odfr$val<-log(odfr$val);
            if (!is.null(odfr$uci)) {odfr$uci<-log(odfr$uci);}
            if (!is.null(odfr$lci)) {odfr$lci<-log(odfr$lci);}
        }
    }
    odfr$type<-'observed';
    odfr<-odfr[,c('type','x','m','s','y','val','lci','uci')];
    
    #model predictions
    mdfr<-NULL;
    if (!is.null(mod)){
        mdfr<-reshape2::melt(mod$data,value.name='val');
        mdfr$facs<-paste(mdfr$x,mdfr$m,mdfr$s)
        mdfr<-mdfr[mdfr$facs %in% facs,]
        if (logscale){mdfr$val<-log(mdfr$val);}
    }
    mdfr$type<-'estimated';
    mdfr$lci<-NA;
    mdfr$uci<-NA;
    mdfr<-mdfr[,c('type','x','m','s','y','val','lci','uci')];
    
    cdfr<-rbind(odfr,mdfr)
    cdfr$class<-paste(cdfr$m,cdfr$s,sep=", ");#add 'class' column for maturity+shell condition
    
#     print(names(cdfr));
#     print(cdfr);
    
    pd<-position_identity()
    p <- ggplot(aes_string(x='y',y='val',colour='type',fill='type',shape='type',linetype='type'),data=cdfr)
    p <- p + scale_linetype_manual(values=c(observed=3,estimated=1))
    p <- p + scale_shape_manual(values=c(observed=19,estimated=1))
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),position=pd,width=0.8,linetype=1)
    p <- p + geom_point(position=pd,size=3)
    p <- p + geom_line( position=pd,size=1,alpha=1)
    if (!is.null(xlim)) p <- p + xlim(xlim);
    if (!is.null(xlab)) p <- p + xlab(xlab)
#    if (!is.null(ylim)) p <- p + ylim(ylim)
    if (!is.null(ylab)) p <- p + ylab(ylab)
    p <- p + facet_grid(class~x)
    p <- p + guides(linetype=guide_legend('type',order=1),
                    shape   =guide_legend('type',order=1),
                    fill    =guide_legend('type',order=1),
                    colour  =guide_legend('type',order=1))
    if (!is.null(label)) p <- p + ggtitle(gsub("_"," ",label,fixed=TRUE));
    p <- p + ggtheme;
    if (showPlot) print(p)
    
    cat("---Done running plotAggregateCatchDataGG(...)\n\n");
    return(invisible(p));
}
