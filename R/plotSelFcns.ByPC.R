#'
#'@title Plot selectivity functions by parameter combination.
#'
#'@description Function to plot selectivity functions by parameter combination.
#'
#'@param res - a TCSAM2015 model report object (class='tcsam2015')
#'@param by - number of functions to include in one page
#'@param nperpage - number of plots per page
#'@param pchs - vector of point symbol ids
#'@param clrs - vector of colors to use
#'
#'@import graphics
#'
#'@export
#'
plotSelFcns.ByPC<-function(res,
                           by=5,
                           nperpage=2,
                           pchs=c(21,22,23,24,25),
                           clrs=c("black","blue","green","cyan","grey")){
    if (class(res)!="tcsam2015"){
        cat("Input object 'res' does not appear to be a TCSAM2015 model results object",
            "Aborting...\n",sep='\n')
        return(NULL);
    }
    
    sels<-res$sel.funcs$sel_cz;
    np<-nrow(sels);
    
    zs<-as.numeric(colnames(sels));
    
    old.par<-par(mfcol=c(nperpage,1),mar=c(2,4,1,1));
    on.exit(par(old.par))
    for (p in seq.int(1,np,by)){
        sel<-sels[p,];
        plot(zs,sel,xlab='Size (mm CW)',ylab='Selectivity',ylim=c(0,1.2),type='n');
        points(zs,sel,col=clrs[1],pch=pchs[1],cex=0.8)
        lines(zs,sel,col=clrs[1],lty=1,lwd=2);
        leg.txt<-c(paste("PC",p));
        leg.pchs<-pchs[1];
        leg.clrs<-clrs[1];
        leg.ltys<-1;
        if (by>1){
            for (pp in 1:(by-1)) {
                if ((p+pp)<=np){
                    sel<-sels[p+pp,]
                    points(zs,sel,col=clrs[pp+1],pch=pchs[pp+1],cex=0.8)
                    lines(zs,sel,col=clrs[pp+1],lty=1,lwd=2);
                    leg.txt<-c(leg.txt,paste("PC",p+pp))
                    leg.pchs<-c(leg.pchs,pchs[pp+1]);
                    leg.clrs<-c(leg.clrs,clrs[pp+1]);
                    leg.ltys<-c(leg.ltys,1);
                }
            }
        }
        legend("topleft",leg.txt,col=leg.clrs,lty=leg.ltys,pch=leg.pchs,cex=0.8)
    }
}