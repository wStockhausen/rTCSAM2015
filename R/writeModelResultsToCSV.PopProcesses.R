#'
#'@title Write population processes from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@description Function to write population processes from model results from TCSAM2015 and rsimTCSAM model runs to a csv file.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param csv - base name for csv files to write to
#'@param verbose - flag (T/F) to print debug info
#'
#'@return nothing
#'
#'@details none.
#'
#'@export
#'
writeModelResultsToCSV.PopQuants<-function(tcsams=NULL,
                                           rsims=NULL,
                                           csv="ModelResults.PopProcesses",
                                           verbose=FALSE){
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    plots<-list();
    
    #natural mortality
    if (verbose) cat("Plotting natural mortality info\n");
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
    write.csv(mdfr,file=paste0(csv,".NatMort.csv"),row.names=TRUE);

    #mean growth increments
    if (verbose) cat("Plotting mean growth increments\n");
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
    write.csv(mdfr,file=paste0(csv,".MeanGrowthIncrements.csv"),row.names=TRUE);

    #molt-to-maturity
    if (verbose) cat("Plotting molt to maturity info\n");
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
    write.csv(mdfr,file=paste0(csv,".prM2M.csv"),row.names=TRUE);

    #recruitment size distribution
    if (verbose) cat("Plotting recruitment size distribution\n");
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
    write.csv(mdfr,file=paste0(csv,".RecDist.csv"),row.names=TRUE);

    #recruitment sex ratio
    if (verbose) cat("Plotting recruitment sex ratio\n");
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
    write.csv(mdfr,file=paste0(csv,".SexRatio.csv"),row.names=TRUE);

    #initial size distribution
    if (verbose) cat("Plotting initial size distribution\n");
    path<-'mr/iN_xmsz';
    csvp<-paste0(csv,".InitialNatZ.csv");
    extractModelResults.RepObjs(tcsams,rsims,path,
                                label.value="Abundance (millions)",
                                cast.formula="x+m+s+z",csv=csvp);

    if (verbose) cat("Done!\n");
}