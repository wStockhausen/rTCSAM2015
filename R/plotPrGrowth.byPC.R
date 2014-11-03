#'
#'@title Plot pr(z'|x,z) estimates by parameter combination.
#'
#'@description Function to plot pr(z'|x,z) estimates by parameter combination.
#'
#'@param res - model results list from TCSAM 2015 model
#'
#'@export
#'
plotPrGrowth.ByPC<-function(res){
    prG<-res$pop.quants$prGr.czz;
    
    prG.dims<-dim(prG);
    prG.nms<-dimnames(prG);
    zs<-as.numeric(prG.nms[[2]]);
    
    npc<-prG.dims[1];
    
    for (pc in 1:npc){
        plotMatrix(prG[pc,,],title=paste("Growth Matrix (",pc,")",sep=''),
                   zlab="pr(post-molt size|pre-molt size)",
                   xlab="pre-molt size (mm CW)",
                   ylab="post-molt size (mm CW)");
    }
    
#     
#     barplot(mat,beside=TRUE,legend.text=m.nms[[1]],
#             names.arg=rep(m.nms[[3]],m.dims[2]));
}