#'
#'@title Plot pr(mature|z) estimates by parameter combination.
#'
#'@description Function to plot pr(mature|z) estimates by parameter combination.
#'
#'@param res - model results list from TCSAM 2015 model
#'
#'@export
#'
plotPrMaturity.ByPC<-function(res){
    prM<-res$pop.quants$prMat.cz;
    
    prM.dims<-dim(prM);
    prM.nms<-dimnames(prM);
        
    plotBars(t(prM),stack=FALSE,groupwidth=100,
             xloc='C',xlabs=as.numeric(prM.nms[[2]]),xlab='size (mm CW)',
             ylab='pr(mature|size)',ylim=c(0,1),
             plotLegend=TRUE,leg.loc="topleft",leg.text=prM.nms[[1]]);
    
#     
#     barplot(mat,beside=TRUE,legend.text=m.nms[[1]],
#             names.arg=rep(m.nms[[3]],m.dims[2]));
}