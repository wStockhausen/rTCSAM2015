#'
#'@title Plot aggregate catch data
#'
#'@description Plot aggregate (numbers or biomass) catch data from fisheries or surveys
#'
#'@param name - fishery or survey name
#'@param obs - list of observed data 
#'@param mod - list of model-predicted data
#'@param logscale - flag (T/F) to plot on ln-scale
#'@param pch - vector of point types to use (for observed data)
#'@param lty - vector of line types to use (for model-predicted data)
#'@param lwd - vector of line widths
#'@param col - vector of colors to use for points and lines
#'@param xlab - x axis title
#'@param ylab - y axis title
#'@param xlim - x axis limits
#'@param ylim - y axis limits
#'@param plot.legend - flag (T/F) to include a legend
#'
#'@export
#'@import graphics
#'
plotAggregateCatchData<-function(name=NULL,
                                 obs=NULL,
                                 mod=NULL,
                                 logscale=FALSE,
                                 pch=c(21,22,23),
                                 lty=c(2,1,1),
                                 lwd=c(2,2,2),
                                 col=c("green","blue","black"),
                                 xlab='year',
                                 ylab='',
                                 xlim=NULL,
                                 ylim=NULL,
                                 plot.legend=TRUE){
    nx<-0;
    yrs<-c();
    abd<-c();
    if (!is.null(obs)){
        nx<-max(nx,length(rownames(obs$data)),na.rm=TRUE);
        yrs<-c(yrs,obs$years);
        abd<-c(abd,as.vector(obs$data));
    }
    if (!is.null(mod)){
        nx<-max(nx,length(rownames(obs$data)),na.rm=TRUE);
        yrs<-c(yrs,mod$years);
        abd<-c(abd,as.vector(mod$data));
    }
    ryrs<-range(yrs);
    if (is.null(ylim)) {
        if (!logscale){ylim<-c(0,max(abd,na.rm=TRUE));}
        else {ylim<-range(log(abd),na.rm=TRUE,finite=TRUE);}
    }
    
    col=rep(col,length.out=nx);
    lwd=rep(lwd,length.out=nx);
    pch=rep(pch,length.out=nx);
    lty=rep(lty,length.out=nx);
    
    units<-gsub("_"," ",tolower(mod$units));
    ylab<-paste(ylab," (",units,")",sep='');
    
    if (logscale){ylab<-paste(ylab," [ln-scale]");}
    
    plot(ryrs,ylim,type='n',xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim);
    
    if (!is.null(obs)){
        sxs<-rownames(obs$data);
        for (x in 1:length(sxs)){
            yv<-NA+ryrs[1]:ryrs[2];            
            uv<-NA+ryrs[1]:ryrs[2];
            lv<-NA+ryrs[1]:ryrs[2];
            for (iy in 1:length(obs$dat[x,])){
                yr<-obs$years[iy];
                yrp<-yr-ryrs[1]+1;
                yv[yrp]<-obs$dat[x,iy];
                if (logscale){
                    uv[yrp]<- 1.96*sqrt(log(1+obs$cv[x,iy]^2));
                    lv[yrp]<--1.96*sqrt(log(1+obs$cv[x,iy]^2));
                } else {
                    uv[yrp]<-obs$dat[x,iy]*(exp( 1.96*sqrt(log(1+obs$cv[x,iy]^2)))-1);
                    lv[yrp]<-obs$dat[x,iy]*(exp(-1.96*sqrt(log(1+obs$cv[x,iy]^2)))-1);
                }
            }
#            cat(x,'- obs: \n')
#            print(yv);
            if (logscale) {
                plotErrorBars.V(ryrs[1]:ryrs[2],log(yv),upper=uv,lower=lv,col=col[x],pch=pch[x])
                points(ryrs[1]:ryrs[2],log(yv),col=col[x],pch=pch[x]);
#                text(ryrs[1]:ryrs[2],log(yv),ryrs[1]:ryrs[2]);
            } else {
                plotErrorBars.V(ryrs[1]:ryrs[2],yv,upper=uv,lower=lv,col=col[x],pch=pch[x])
                points(ryrs[1]:ryrs[2],yv,col=col[x],pch=pch[x]);
#                text(ryrs[1]:ryrs[2],yv,ryrs[1]:ryrs[2]);
            }
        }
    }
    if (!is.null(mod)){
        sxs<-rownames(mod$data);
        for (x in 1:length(sxs)){
            yv<-NA+ryrs[1]:ryrs[2];            
            for (iy in 1:length(mod$dat[x,])){
                yr<-mod$years[iy];
                yrp<-yr-ryrs[1]+1;
                yv[yrp]<-mod$dat[x,iy]
            }
#            cat(x,'- mod: \n')
#            print(yv);
            if (logscale) {
                lines(ryrs[1]:ryrs[2],log(yv),col=col[x],lty=lty[x],lwd=lwd[x]);
#                text(ryrs[1]:ryrs[2],log(yv),ryrs[1]:ryrs[2]);
            } else {
                lines(ryrs[1]:ryrs[2],yv,col=col[x],lty=lty[x],lwd=lwd[x]);
#                text(ryrs[1]:ryrs[2],yv,ryrs[1]:ryrs[2]);
            }
        }
    }        
    if (is.null(name)) {name<-'';}
    mtext(name,side=3,line=0,outer=FALSE,adj=0.05);
    if (plot.legend){
        legend("topright",rownames(mod$data),
               cex=0.7,col=col,lty=lty,lwd=lwd,pch=pch);
    }
}
