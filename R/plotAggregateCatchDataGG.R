#'
#'@title Plot aggregate catch data
#'
#'@description Plot aggregate (numbers or biomass) catch data from fisheries or surveys
#'
#'@param name - fishery or survey name
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
#'@import reshape2
#'
#'@export
#'
plotAggregateCatchDataGG<-function(name=NULL,
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
                                 showPlot=TRUE){
    
    units<-gsub("_"," ",tolower(mod$units));
    ylab<-paste(ylab," (",units,")",sep='');
    
    if (logscale){ylab<-paste(ylab," [ln-scale]");}
    
    ci<-c((1-ci)/2,1-(1-ci)/2);
    
    #observations
    odfr<-NULL;
    if (!is.null(obs)){
        odfr<-melt(obs$data,value.name='val');
        cvs <-melt(obs$cvs,value.name='cv');
        odfr<-cbind(odfr,cv=cvs$cv);
        if (tolower(pdfType)=='normal'){
            #normal
            sd<-odfr$cv*odfr$val;#sd on arithmetic scale
            odfr$sd<-sd;
            odfr$lci<-qnorm(ci[1],mean=odfr$val,sd=sd);
            odfr$uci<-qnorm(ci[2],mean=odfr$val,sd=sd);
        } else if (tolower(pdfType)=='lognormal'){
            #lognormal
            sd<-sqrt(log(1+odfr$cv^2));#sd on ln-scale
            odfr$sd<-odfr$cv*odfr$val; #sd on arithmetic scale
            odfr$lci<-odfr$val*exp(dnorm(ci[1],mean=log(odfr$val),sd=sd));
            odfr$uci<-odfr$val*exp(dnorm(ci[2],mean=log(odfr$val),sd=sd));
        }
        odfr$facs<-paste(odfr$sex,odfr$maturity,odfr$`shell condition`)
        #find factor combinations which are NOT all 0's
        tots<-dcast(odfr,sex+maturity+`shell condition`~.,value.var='val',fun.aggregate=sum,drop=TRUE)
        tots<-tots[!(tots$`.`==0),]
        facs<-paste(tots$sex,tots$maturity,tots$`shell condition`)
        odfr<-odfr[odfr$facs %in% facs,]
        if (logscale){
            odfr$val<-log(odfr$val);
            odfr$uci<-log(odfr$uci);
            odfr$lci<-log(odfr$lci);
        }
    }
    
    #model predictions
    mdfr<-NULL;
    if (!is.null(mod)){
        mdfr<-melt(mod$data,value.name='val');
        mdfr$facs<-paste(mdfr$sex,mdfr$maturity,mdfr$`shell condition`)
        mdfr<-mdfr[mdfr$facs %in% facs,]
        if (logscale){mdfr$val<-log(mdfr$val);}
    }
    
    pd<-position_dodge(0.2)
    p <- ggplot(aes_string(x='year',y='val',colour='maturity',shape='`shell condition`',fill='maturity'),data=odfr)
    p <- p + geom_errorbar(aes_string(ymin='lci',ymax='uci'),width=1,position=pd)
    p <- p + geom_point(position=pd,size=3)
    p <- p + geom_line(position=pd,size=1,linetype=3,alpha=0.5)
    p <- p + geom_line(data=mdfr,position=pd,size=1,linetype=1,alpha=1.0)
    if (!is.null(ylab)) p <- p + ylab(ylab)
    if (!is.null(ylim)) p <- p + ylim(ylim)
    p <- p + facet_wrap(~sex,ncol=1)
    p <- p + ggtitle(name)
    if (showPlot) print(p)
    
    return(p)
}