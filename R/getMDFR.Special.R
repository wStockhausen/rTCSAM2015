#'
#'@title Get natural mortality rates from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get natural mortlaity rates from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param type - flag indicating which M's to extract ('M_cxm', 'M_yxm' or 'M_yxmsz') 
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts natural mortality rates
#'
#'@export
#'
getMDFR.NatMort<-function(tcsams=NULL,rsims=NULL,
                          type=c('M_cxm','M_yxm','M_yxmsz'),
                          verbose=FALSE){
    if (verbose) cat("--Getting natural mortality info\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    if (!is.null(tcsams)){
        if (type[1]=='M_cxm'){
            mdfr<-getMDFR('mp/M_cxm',tcsams,NULL);
            mdfr$y<-'';
            ums<-as.character(unique(mdfr$model))
            for (um in ums){
                tcsam<-tcsams[[um]];
                pgi<-tcsam$mpi$nm$pgi;
                nPCs<-length(pgi$pcs)-1;
                for (pc in 1:nPCs){
                    idx<-(mdfr$pc==pc)&(mdfr$model==um);
                    mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                    mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims)
                }
            }
            mdfr$pc<-mdfr$y;
            mdfr<-mdfr[,c("modeltype","model","pc","x","m","val")];
        } else if (type[1]=='M_yxm'){
            mdfr<-getMDFR('mp/M_yxmsz',tcsams,NULL);
            mdfr<-reshape2::dcast(mdfr,formula='modeltype+model+y+x+m~.',fun.aggregate=mean,value.var='val');
            names(mdfr)<-c("modeltype","model","y","x","m","val");
            
        } else if (type[1]=='M_yxmsz'){
            mdfr<-getMDFR('mp/M_yxmsz',tcsams,NULL);
            mdfr<-mdfr[,c("modeltype","model","y","x","m","s","z","val")];
        }
    }
    
    if (!is.null(rsims)){
        if (type[1]=='M_cxm'){
            mdfrp<-getMDFR('mp/M_cxm',NULL,rsims);
            ums<-as.character(unique(mdfrp$model))
            for (um in ums){
                idx<-(mdfrp$model==um);
                mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims)
            }
            mdfrp<-mdfrp[,c("modeltype","model","pc","x","m","val")];
        } else if (type[1]=='M_yxm'){
            mdfrp<-getMDFR('mp/M_yxmsz',rsimms,NULL);
            mdfrp<-reshape2::dcast(mdfrp,formula='modeltype+model+y+x+m~.',fun.aggregate=mean,value.var='val');
            names(mdfrp)<-c("modeltype","model","y","x","m","val");
        } else if (type[1]=='M_yxmsz'){
            mdfrp<-getMDFR('mp/M_yxmsz',rsims,NULL);
            mdfrp<-mdfr[,c("modeltype","model","y","x","m","s","z","val")];
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
##-----------------------
#'
#'@title Get mean growth increments from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get mean growth increments from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts mean growth increments.
#'
#'@export
#'
getMDFR.MeanGrowthIncrements<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting mean growth increments\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    if (!is.null(tcsams)){
        mdfr<-getMDFR('mp/T_list/mnZAM_cz',tcsams,NULL);
        mdfr$y<-'';
        mdfr$x<-'';
        ums<-as.character(unique(mdfr$model))
        for (um in ums){
            tcsam<-tcsams[[um]];
            pgi<-tcsam$mpi$grw$pgi;
            nPCs<-length(pgi$pcs)-1;#last element is a NULL
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$model==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
            }
        }
        mdfr$pc<-mdfr$y
        mdfr<-mdfr[,c('modeltype','model','pc','x','z','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/T_list/mnZAM_cxz',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims)
        }
        mdfrp<-mdfrp[,c('modeltype','model','pc','x','z','val')];
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
##------------------------
#'
#'@title Get molt-to-maturity ogives from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get molt-to-maturity ogives from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts molt-to-maturity ogives.
#'
#'@export
#'
getMDFR.prM2M<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting molt-to-maturity ogives.\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    if (!is.null(tcsams)){
        mdfr<-getMDFR('mp/prM2M_cz',tcsams,NULL);
        mdfr$y<-'';
        mdfr$x<-'';
        ums<-as.character(unique(mdfr$model))
        for (um in ums){
            tcsam<-tcsams[[um]];
            pgi<-tcsam$mpi$mat$pgi;
            nPCs<-length(pgi$pcs)-1;#last element is a NULL
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$model==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
            }
        }
        mdfr$pc<-mdfr$y;
        mdfr<-mdfr[,c('modeltype','model','pc','x','z','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/prM2M_cxz',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfrp<-mdfrp[,c('modeltype','model','pc','x','z','val')];
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
##------------------------
#'
#'@title Get recruitment size distribution from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get recruitment size distribution from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts mean growth increments.
#'
#'@export
#'
getMDFR.RecSizeDistribution<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting recruitment size distribution.\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    path<-'mp/R_list/R_cz';
    if (!is.null(tcsams)){
        mdfr<-getMDFR(path,tcsams,NULL);
        mdfr$y<-'';
        ums<-as.character(unique(mdfr$model))
        for (um in ums){
            tcsam<-tcsams[[um]];
            pgi<-tcsam$mpi$rec$pgi;
            nPCs<-length(pgi$pcs)-1;#last element is a NULL
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$model==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
            }
        }
        mdfr$pc<-mdfr$y;
        mdfr<-mdfr[,c('modeltype','model','pc','z','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR(path,NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfrp<-mdfrp[,c('modeltype','model','pc','z','val')];
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
##------------------------
#'
#'@title Get recruitment sex ratio from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get recruitment sex ratio from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts recruitment sex ratio.
#'
#'@export
#'
getMDFR.SexRatio<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting recruitment sex ratio.\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    
    path<-'mp/R_list/Rx_c';
    if (!is.null(tcsam)){
        mdfr<-getMDFR(path,tcsams,NULL);
        mdfr$y<-'';
        ums<-as.character(unique(mdfr$model))
        for (um in ums){
            tcsam<-tcsams[[um]];
            pgi<-tcsam$mpi$rec$pgi;
            nPCs<-length(pgi$pcs)-1;#last element is a NULL
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$model==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
            }
        }
        mdfr$pc<-mdfr$y;
        mdfr<-mdfr[,c('modeltype','model','pc','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR(path,NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfrp<-mdfrp[,c('modeltype','model','pc','val')];
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
##-----------------
#'
#'@title Get growth transition matrices from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get growth transition matrices from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return data.frame
#'
#'@details Extracts recruitment sex ratio.
#'
#'@export
#'
getMDFR.GrowthTransitionMatrices<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting growth transition matrices.\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    if (!is.null(tcsams)){
        mdfr<-getMDFR('mp/T_list/T_czz',tcsams,NULL);
        mdfr$y<-'';
        mdfr$x<-'';
        ums<-as.character(unique(mdfr$model))
        for (um in ums){
            tcsam<-tcsams[[um]];
            pgi<-tcsam$mpi$grw$pgi;
            nPCs<-length(pgi$pcs)-1;#last element is a NULL
            for (pc in 1:nPCs){
                idx<-(mdfr$pc==pc)&(mdfr$model==um);
                mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
                mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
                mdfr$y[idx]<-reformatTimeBlocks(mdfr$y[idx],tcsam$mc$dims);
            }
        }
        mdfr$pc<-mdfr$y
        mdfr<-mdfr[,c('modeltype','model','pc','x','z','zp','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/T_list/T_cxzz',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
            mdfrp<-mdfrp[,c('modeltype','model','pc','x','z','zp','val')];
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
