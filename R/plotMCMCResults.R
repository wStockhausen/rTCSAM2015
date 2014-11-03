#'
#'@title Plot compiled MCMC results.
#'
#'@description Function to plot compiled MCMC results.
#'
#'@param lst - list of the compiled MCMC results to plot
#'
#'@export
#'
plotMCMCResults<-function(lst){
    
    nms<-names(lst);#names of variables to plot
    for (nm in nms){
        cat('\tChecking',nm,'\n')
        r<-lst[[nm]];#mcmc result corresponding to nm
        if (!is.null(r)){
            if (is.numeric(r)){
                #numeric quantity
                cat('is numeric\n')
                if (is.vector(r)){
                    #variable is a number, compiled mcmc results are a vector
                    cat('mcmc results are a vector\n')
                    plotMCMCResults.forVector(r,label=nm,add=FALSE,plotEst=TRUE);
                } else if (is.matrix(r)){
                    #variable is a vector, compiled mcmc results are a matrix
                    cat('mcmc results are a matrix\n');
                    plotMCMCResults.forMatrix(r,label=nm,add=FALSE,plotEst=TRUE)
                } else if (is.array(r)){
                    d<-dim(r);
                    if (length(d)==3){
                        #variable is a matrix, compiled mcmc results are a 3-d array
                        cat('mcmc results are a 3-d array\n')
                        plotMCMCResults.forArray3D(r,label=nm,add=FALSE,plotEst=TRUE)
                    }
                }
            } else if (is.list(r)){
                #list quantity (e.g., model parameters)
            }
        }
    }
}