#'
#'@title Get natural mortality rates from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get natural mortlaity rates from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
#'
#'@details Extracts natural mortality.
#'
#'@export
#'
getMDFR.NatMort<-function(tcsams,rsims,verbose=FALSE){
    if (verbose) cat("--Getting natural mortality info\n");
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    mdfr<-NULL;
    if (!is.null(tcsams)){
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
        mdfr<-subset(mdfr,select=-y)
    }
    
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/M_cxm',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims)
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
#'@return nothing
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
#'@return nothing
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
        mdfr<-getMDFR('mp/prMolt2Mat_cz',tcsams,NULL);
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
        mdfrp<-getMDFR('mp/prMolt2Mat_cxz',NULL,rsims);
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
#'@return nothing
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
        mdfr<-mdfr[,c('modeltype','model','pc','x','z','val')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR(path,NULL,rsims);
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
#'@title Get recruitment sex ratio from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe
#'
#'@description Function to get recruitment sex ratio from model results from TCSAM2015 and rsimTCSAM model runs as a dataframe.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
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
        mdfr<-mdfr[,c('pc','val','model','modeltype')];
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
#'@return nothing
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
        mdfr<-mdfr[,c('pc','x','z','zp','val','model','modeltype')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/T_list/T_cxzz',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims)
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    
    if (verbose) cat("--Done. \n");
    return(mdfr);
}
