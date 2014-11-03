#'
#'@title Compiles MCMC results from a TCSAM2015 model run.
#'
#'@description Function to compile MCMC results from a TCSAM2015 model run.
#'
#'@param mcmc - object of class 'tcsam2015mcmc' (the result of sourcing a TCSAM2015 MCMC results file)
#'
#'@return list by MCMC results compiled by model 'object' (e.g., parameter or other named MCMC output)
#'
#'@export
#'
compileMCMCResults<-function(mcmc){
    if (class(mcmc)!='tcsam2015mcmc'){
        cat('Error in compileMCMCResults(...).\n',
            "Object 'mcmc' is not of class 'tcsam2015mcmc'\n",
            "Returning NULL\n");
        return(NULL);
    }
    r1<-mcmc[[1]];
    nr<-length(mcmc);
    vs<-names(r1);
    
    lst<-vector(mode='list',length=length(vs));
    names(lst)<-vs;
    for (v in vs){        
        r1v<-r1[[v]];
        if (is.null(r1v)){
            cat(v,'is NULL\n')
            lst[[v]]<-NULL;
        } else if (is.numeric(r1v)){
            #numeric variable
            cat(v,'is a numeric variable\n')
            d1v<-dim(r1v);
            if (length(d1v)==0){
                #v is a number
                lst[[v]]<-vector(mode='numeric',length=nr);
                for (r in 1:nr){
                    lst[[v]][r]<-mcmc[[r]][[v]];
                }
            } else if (length(d1v)==1){
                #v is a vector
                lst[[v]]<-matrix(nrow=d1v,ncol=nr);
                rownames(lst[[v]])<-names(r1v);
                for (r in 1:nr){
                    lst[[v]][,r]<-mcmc[[r]][[v]];
                }
            } else  if (length(d1v)==2){
                #v is a matrix
                lst[[v]]<-array(dim=c(d1v,nr),dimnames=list(dimnames(r1v)[[1]],dimnames(r1v)[[2]],rep=1:nr));
                for (r in 1:nr){
                    lst[[v]][,,r]<-mcmc[[r]][[v]];
                }
            }
        } else if (is.list(r1v)){
            #list variable
            cat(v,'is a list variable\n')
            pcsv<-names(r1v); #parameter combinations
            npcv<-length(r1v);#number of parameter combinations
            sublst<-vector(mode='list',length=npcv);
            for (pc in pcsv){
                r1vp<-r1v[[pc]];                
                if (is.numeric(r1vp)){
                    #numeric variable
                    d1vp<-dim(r1vp);
                    if (length(d1vp)==0){
                        cat("found a number\n")
                        #v is a number
                        sublst[[pc]]<-vector(mode='numeric',length=nr);
                        for (r in 1:nr){
                            sublst[[pc]][r]<-mcmc[[r]][[v]][[pc]];
                        }
                    } else if (length(d1vp)==1){
                        cat("found a vector\n")
                        #vector
                        sublst[[pc]]<-matrix(nrow=d1vp,ncol=nr);
                        rownames(sublst[[pc]])<-names(r1vp);
                        for (r in 1:nr){
                            sublst[[pc]][,r]<-mcmc[[r]][[v]][[pc]];
                        }
                    } else  if (length(d1vp)==2){
                        cat("found a matrix\n")
                        #matrix
                        sublst[[pc]]<-array(dim=c(d1vp,nr),dimnames=list(dimnames(r1vp)[[1]],dimnames(r1vp)[[2]],rep=1:nr));
                        for (r in 1:nr){
                            sublst[[pc]][,,r]<-mcmc[[r]][[v]][[pc]];
                        }
                    }
                }#is.numeric(r1p)
            }#pc loop
            lst[[v]]<-sublst;
        }#is.list(r1v)
    }#v loop
    return(invisible(lst));
}