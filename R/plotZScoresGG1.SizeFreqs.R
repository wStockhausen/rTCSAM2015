#'
#'@title Plot residuals from size frequency fits
#'
#' @description  Plot Pearson's residuals or negative log-likelihood components
#' from size frequency fits using ggplot2.
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
#' @export
#' 
plotZScoresGG1.SizeFreqs<-function(fits,
                                   mc,
                                   sxs=c(mc$dims$x$nms,"ALL_SEX"),
                                   mss=c(mc$dims$m$nms,"ALL_MATURITY"),
                                   scs=c(mc$dims$s$nms,"ALL_SHELL"),
                                   label="",
                                   ggtheme=theme_grey(),
                                   showPlot=TRUE,
                                   verbose=FALSE){
    if (verbose) cat("---Running plotFitsGG1.SizeComps(...) for",label,"\n");
    
    label<-gsub("[_]"," ",label);#replace "_"'s with blank spaces
    
    dims<-mc$dims;
    sxs<-gsub("_"," ",tolower(sxs),fixed=TRUE);
    mss<-gsub("_"," ",tolower(mss),fixed=TRUE);
    scs<-gsub("_"," ",tolower(scs),fixed=TRUE);
    
    n<-length(fits);
    yrs<-min(as.numeric(names(fits)),na.rm=TRUE):max(as.numeric(names(fits)),na.rm=TRUE)
    zbs<-dims$z$vls;
    
    dms<-c(length(sxs),length(mss),length(scs),length(yrs),length(zbs));
    dmnames<-list(x=sxs,m=mss,s=scs,y=yrs,z=zbs);
    pAtZ<-array(0,dms,dmnames);#pearson's residuals
    nAtZ<-array(0,dms,dmnames);#nll's residuals
    
    yrsp<-names(fits);
    for (i in 1:(n-1)){
        fit<-fits[[i]];
        x<-gsub("_"," ",tolower(fit$x),fixed=TRUE);
        m<-gsub("_"," ",tolower(fit$m),fixed=TRUE);
        s<-gsub("_"," ",tolower(fit$s),fixed=TRUE);
        y<-yrsp[i];
        pAtZ[x,m,s,y,]<-fit$fit$zscrs;
        nAtZ[x,m,s,y,]<-fit$fit$nlls;
    }
    
    pdfr<-reshape2::melt(pAtZ,value.name='residual');
    ndfr<-reshape2::melt(nAtZ,value.name='residual');
    
    #remove all-zero factor combinations
    for (x in sxs){
        for (m in mss){
            for (s in scs){
                pAtZp<-t(as.matrix(pAtZ[x,m,s,,]));
                if (sum(abs(pAtZp),na.rm=TRUE)==0){
                    idx<-(pdfr$x==x)&(pdfr$m==m)&(pdfr$s==s);
                    pdfr<-pdfr[!(idx),];
                    idx<-(ndfr$x==x)&(ndfr$m==m)&(ndfr$s==s);
                    ndfr<-ndfr[!idx,];
                } else {
                    if (verbose) cat('Will plot factor combination',x,m,s,"\n")
                }
            }#s
        }#m
    }#x
                    
#    print(pdfr);
    
    #extract pearsons residuals and plot
    pdfr$type<-'O > M';
    pdfr$type[pdfr$residual<0]<-'O < M';
    pdfr$residual<-abs(pdfr$residual);
    pr<-plotCompsAsCirclesGG(pdfr,
                            x='y',y='z',z='residual',
                            category='type',
                            faceting='x~m+s',
                            title=paste(label,": Pearson's residuals",sep=''),
                            xlab="year",
                            ylab="size (mm CW)",
                            alpha=0.6,
                            ggtheme=ggtheme,
                            showPlot=showPlot,
                            verbose=verbose);
    
    #extract nlls and plot
    ndfr$type<-'O > M';
    ndfr$type[ndfr$residual<0]<-'O < M';
    ndfr$residual<-abs(ndfr$residual);
    pn<-plotCompsAsCirclesGG(ndfr,
                            x='y',y='z',z='residual',
                            category='type',
                            faceting='x~m+s',
                            title=paste(label,": negative loglikeihoods",sep=''),
                            xlab="year",
                            ylab="size (mm CW)",
                            alpha=0.6,
                            ggtheme=ggtheme,
                            showPlot=showPlot,
                            verbose=FALSE);
                    
    if (verbose) cat("---Done running plotFitsGG1.SizeComps(...)\n\n");
    return(invisible(list(pearsons=pr,nlls=pn)));
}