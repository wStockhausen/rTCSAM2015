#'
#'@title Plot mean size comps fits
#'
#' @description  Function to plot mean size comps fits using ggplot2.
#' 
#' @param fits - list of fits
#' @param mc - model configuration list
#' @param sxs - character vector of sexes to plot
#' @param mss - character vector of maturity states to plot
#' @param scs - character vector of shell conditions to plot
#' @param label - plot label
#' @param ggtheme - ggplot2 theme
#' @param showPlot - flag to show (print) plot immediately on current graphics device
#' @param verbose - flag (T/F) to print dagnostic info
#' 
#' @return list of list of ggplot2 plot objects
#' 
#' @import ggplot2
#' 
#' @export
#' 
plotFitsGG.MeanSizeComps<-function(fits,
                                mc,
                                sxs=c(mc$dims$x$nms,"ALL_SEX"),
                                mss=c(mc$dims$m$nms,"ALL_MATURITY"),
                                scs=c(mc$dims$s$nms,"ALL_SHELL"),
                                label="",
                                ggtheme=theme_grey(),
                                showPlot=TRUE,
                                verbose=FALSE){
    if (verbose) cat("---Running plotFitsGG.MeanSizeComps(...) for",label,"\n");
    
    label<-gsub("[_]"," ",label);#replace "_"'s
    
    dims<-mc$dims;
    sxs<-gsub("_"," ",tolower(sxs),fixed=TRUE);
    mss<-gsub("_"," ",tolower(mss),fixed=TRUE);
    scs<-gsub("_"," ",tolower(scs),fixed=TRUE);
    
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    zbs<-dims$z$vls;
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(sx=sxs,ms=mss,sc=scs,yr=yrs,zb=zbs);
    oAtZ<-array(0,dms,dmnames);#observed size comps
    mAtZ<-array(0,dms,dmnames);#model size comps
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        oAtZ[x,m,s,y,]<-fit$fit$obs;
        mAtZ[x,m,s,y,]<-fit$fit$mod;
    }
    
    #melt arrays, add types, stack into single dataframe
    odfr<-reshape2::melt(oAtZ,value.name='comp');
    mdfr<-reshape2::melt(mAtZ,value.name='comp');
    odfr$type<-'observed';
    mdfr$type<-'estimated';
    pdfr<-rbind(odfr,mdfr);
    
    #calculate averages over years
    pdfr<-reshape2::dcast(pdfr,type+sx+ms+sc+zb~.,fun.aggregate=mean,value.var='comp');
    names(pdfr)<-c('type','sx','ms','sc','zb','comp')
    pdfr$fac<-paste(pdfr$ms,pdfr$sc,sep=', ')

    #check normalization
    if (verbose){
        tst<-reshape2::dcast(pdfr,sx+ms+sc~type,fun.aggregate=sum,value.var='comp')
        cat("Normalization check:\n");
        print(tst);
    }
    
    #remove "missing" factor levels
    nr<-nrow(tst);
    for (r in 1:nr){
        if (tst$observed[r]<0.0001){
            idx<-(pdfr$sx %in% tst$sx[r])&(pdfr$ms %in% tst$ms[r])&(pdfr$sc %in% tst$sc[r]);
            pdfr<-pdfr[!idx,];
        }
    }
    
    if (verbose){
        #check normalization again
        tst<-reshape2::dcast(pdfr,sx+ms+sc~type,fun.aggregate=sum,value.var='comp')
        cat("Repeat normalization check:\n");
        print(tst)
        print(names(tst));
    }
    
    #make plots
    odx<-(pdfr$type=='observed');#extraction indices for data
    pl <- ggplot(aes(x=zb,y=comp,color=type,fill=type),data=pdfr);
    pl <- pl + geom_bar(data=pdfr[odx,],alpha=0.8,stat='identity');
    pl <- pl + geom_line(data=pdfr[!odx,],alpha=0.8);
    pl <- pl + labs(x='size (mm)',y='composition');
    pl <- pl + facet_grid(sx~fac);
    pl <- pl + ggtitle(label);
    pl <- pl + guides(fill=guide_legend('type'),colour=guide_legend('type'))
    if (showPlot) print(pl);
    if (verbose) cat("---Done running plotFitsGG.MeanSizeComps(...)\n\n");
    return(invisible(pl))
}
