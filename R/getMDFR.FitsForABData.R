#'
#'@title Get fits to abundance or biomass data as a dataframe
#'
#'@description Function to get fits to abundance or biomass data as a dataframe.
#'
#'@param afits - 
#'@param ci - confidence intervals
#'@param verbose - flag (T/F) to print dagnostic info
#'
#'@return dataframe
#'
#'@details none.
#'
#'@export
#'
getMDFR.FitsForABData<-function(afits,
                                ci=0.95,
                                verbose=FALSE){
    if (verbose) cat("---Running getMDFR.FitsForABData(...).\n");
    
    nf<-length(afits);
    if (verbose) cat("----number of fits =",nf,"\n");
    
    ci<-c((1-ci)/2,1-(1-ci)/2);
    
    mdfr<-NULL;
    
    #observed values
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            pdfType<-nll$nll.type;
            obs<-nll$obs;
            sdv<-nll$stdv;
            if (verbose) cat("----pdfType =",pdfType,"\n")
            if (tolower(pdfType)=='normal'){
                #normal, sdv on arithmetic scale
                if (verbose) cat('----using err type = normal\n')
                lci<-qnorm(ci[1],mean=obs,sd=sdv);
                uci<-qnorm(ci[2],mean=obs,sd=sdv);
            } else if (tolower(pdfType)=='lognormal'){
                #lognormal, sdv on ln-scale
                if (verbose) cat('----using err type = lognormal\n')
                lci<-qlnorm(ci[1],meanlog=log(obs),sdlog=sdv);
                uci<-qlnorm(ci[2],meanlog=log(obs),sdlog=sdv);
            } else if (tolower(pdfType)=='norm2'){
                #normal, sdv on arithmetic scale
                if (verbose) cat('----using err type = normal, but fit uses norm2\n')
                lci<-qnorm(ci[1],mean=obs,sd=sqrt(0.5));
                uci<-qnorm(ci[2],mean=obs,sd=sqrt(0.5));
            } else if (tolower(pdfType)=='none'){
                if (verbose) cat('---using err type = none\n')
                lci<-NULL;
                uci<-NULL;
            } else {
                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                cat('Error in getMDFR.ZScores.\n')
                cat("pdfType '",pdfType,"' not recognized!!\n")
                cat("Exiting function.\n")
                cat('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
                return(NULL)
            }
            if (tolower(pdfType)!='none'){
                dfrp1<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                  y=as.numeric(names(obs)),
                                  val=obs,var="obs");
                dfrp2<-NULL;
                dfrp3<-NULL;
                if (!is.null(uci)){
                    dfrp2<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                      y=as.numeric(names(obs)),
                                     val=lci,var="lci");
                    dfrp3<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                      y=as.numeric(names(obs)),
                                     val=uci,var="uci");
                }
                mdfr<-rbind(mdfr,dfrp1,dfrp2,dfrp3);
            }
        }#!is.null(afit)
    }#n
    
    #predicted values
    for (n in 1:nf){
        afit<-afits[[n]];
        if (!is.null(afit)){
            nll<-afit$nll;
            pdfType<-nll$nll.type;
            if (tolower(pdfType)!='none'){
                dfrp<-data.frame(x=afit$x,m=afit$m,s=afit$s,
                                 y=as.numeric(names(nll$mod)),
                                 val=nll$mod,var='est');
                mdfr<-rbind(mdfr,dfrp);
            }
        }
    }
        
    mdfr$x<-gsub("_"," ",tolower(mdfr$x),fixed=TRUE);
    mdfr$m<-gsub("_"," ",tolower(mdfr$m),fixed=TRUE);
    mdfr$s<-gsub("_"," ",tolower(mdfr$s),fixed=TRUE);
    
    if (verbose) cat("---Done running getMDFR.FitsForABData(...).\n");
    return(mdfr);
}
