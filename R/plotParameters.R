#'
#'@title Plot initial & final values, limits, and priors for model parameters.
#'
#'@description Function to plot initial & final values, limits, and priors for model parameters.
#'
#'@param res - TCSAM2015 model results list object
#'@param res.mcmc - list object for compiled MCMC results
#'@param nv.mfrow - number of rows/page for plotting NumberVector parameters
#'@param vv.mfrow - number of rows/page for plotting VectorVector parameters
#'
#'@import graphics
#'
#'@export
#'
plotParameters<-function(res,
                         res.mcmc=NULL,
                         nv.mfrow=NULL,
                         vv.mfrow=NULL){
    if (is.null(nv.mfrow)){
        if (names(dev.cur())[1]=='pdf'){
            nv.mfrow=c(5,4);#5 rows, 4 cols
        } else {
            nv.mfrow<-c(2,1);#2 rows, 1 col
        }
    }
    if (is.null(vv.mfrow)){
        if (names(dev.cur())[1]=='pdf'){
            vv.mfrow=c(4,1);#4 rows, 1 cols
        } else {
            vv.mfrow<-c(2,1);#2 rows, 1 col
        }
    }
    old.par<-par(mfrow=nv.mfrow,mar=c(2,1,1,1));#value is old par
    nv.par<-par(old.par);#value is par for NumberVector plots
    old.par<-par(mfrow=vv.mfrow,mar=c(3,2,1,1));#value is old par
    vv.par<-par(old.par);#value is par for VectorVector plots
    
    #plot parameters
    gnms<-names(res$mpi);#group names
    for (gnm in gnms){
        g<-res$mpi[[gnm]];#extract parameter group
        nms<-names(g);    #extract parameter names
        nms<-nms[nms!='pgi'];
        old.par<-par(nv.par);
        for (nm in nms){
            p<-g[[nm]]; #extract parameter
            if (!is.null(p)){
                if (is.null(p[[1]]$finalVals)) {
                    #p is a NumberVector
                    cat("Plotting NumberVector parameter",nm,"\n")
                    plotParameters.NumberVector(p,label=nm,mcmc=res.mcmc[[nm]]);
                }
            }
        }#nm
        par(old.par);
        old.par<-par(vv.par)
        for (nm in nms){
            p<-g[[nm]];
            if (!is.null(p)){
                if (!is.null(p[[1]]$finalVals)) {
                    #p is a NumberVector
                    cat("Plotting VectorVector parameter",nm,"\n")
                    plotParameters.VectorVector(p,nm);
                }
            }
        }
        par(old.par)
    }#gnm
}