#'
#'@title Plot regressions of capture rate on effort to assess effort extrapolations.
#'
#'@description Function to plot  regressions of capture rate on effort to assess effort extrapolations.
#'
#'@param repObj - tcsam2015 report object
#'@param showPlot - flag to show plots immediately
#'
#'@return list of ggplot objects (by fishery name) showing regressions
#'
#'@import ggplot2
#'
#'@export
#'
plotEffortRegressions<-function(repObj,showPlot=TRUE){
    ers<-repObj$mp$Eff_list;
    fs<-names(ers);
    plts<-list();
    for (f in fs){
        er<-ers[[f]];
        if (length(er)>0){
            dns<-dimnames(er$cpF_xmsy);
            dfr<-NULL;
            for (x in dns$x){
                for (m in dns$m){
                    for (s in dns$s){
                        dfrp1<-as.data.frame(list(x=x,m=m,s=s,type='data',y=names(er$eff_y),eff=er$eff_y,cpF=er$cpF_xmsy[x,m,s,]));
                        eff<-seq(from=0,to=max(er$eff_y),length.out=2);
                        cpF<-(er$avgFc_xms[x,m,s]/er$avgEff)*eff;
                        dfrp2<-as.data.frame(list(x=x,m=m,s=s,type='fit',y="",eff=eff,cpF=cpF));
                        dfr<-rbind(dfr,dfrp1,dfrp2);
                    }#s
                }#m
            }#x
            #plot dfr
            p <- ggplot(aes(x=eff,y=cpF,colour=type,linetype=s,shape=s),data=dfr);
            p <- p + geom_text(aes(label=y),data=dfr[dfr$type=='data',],size=3)
#            p <- p + geom_point();
            p <- p + geom_line(data=dfr[dfr$type=='fit',])
            p <- p + xlab("Effort (potlifts)");
            p <- p + ylab("fishery capture rate");
            p <- p + facet_grid(m~x,scales="free");
            p <- p + ggtitle(f);
            if (showPlot) print(p);
            plts[[f]]<-p;
        }#f!=""
    }#f
    return (invisible(plts));
}