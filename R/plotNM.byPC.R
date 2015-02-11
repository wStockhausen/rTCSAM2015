#'
#'@title Plot natural mortality estimates by parameter combination.
#'
#'@description Function to plot natural mortality estimates by parameter combination.
#'
#'@param res - model results list from TCSAM 2015 model
#'@param dims - dimension names for natural mortality results
#'
#'@export
#'
plotNM.ByPC<-function(res,dims=c("PC","SEX","MATURITY")){
    M<-res$pop.quants$M.cxm;
    mx<-max(M);
    
    m.dims<-dim(M);
    m.nms<-dimnames(M);
    
    npc<-m.dims[1];
    mat<-as.vector(t(M[1,,]));
    if (npc>1){
        for (pc in 2:npc){
            mat<-rbind(mat,as.vector(t(M[pc,,])));
        }
    }
    
#     plotBars(t(mat),stack=FALSE,groupwidth=80,
#              xloc='C',xlabs=list(rep(m.nms[[3]],m.dims[2]),m.nms[[2]]),xlab='',
#              ylab='Natural Mortality (yr^-1)',
#              plotLegend=TRUE,leg.text=m.nms[[1]]);
    
#     
#     barplot(mat,beside=TRUE,legend.text=m.nms[[1]],
#             names.arg=rep(m.nms[[3]],m.dims[2]));
}