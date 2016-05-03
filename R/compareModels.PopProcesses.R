#'
#'@title Compare population process-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare population process-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsams - single TCSAM2015 model report object, or named list of such
#'@param rsims - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects
#'
#'@details none.
#'
#'@export
#'
compareModels.PopProcesses<-function(tcsams=NULL,
                                      rsims=NULL,
                                      showPlot=TRUE,
                                      pdf=NULL,
                                      width=8,
                                      height=6,
                                      verbose=FALSE){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close())
    }
    
    if (inherits(tcsams,'tcsam2015.rep')){
        tcsams<-list(tcsam=tcsams);#wrap in list
    }
    if (class(rsims)=='rsimTCSAM'){
        rsims<-list(rsim=rsims);#wrap in list
    }
    
    plots<-list();
    
    #natural mortality
    if (verbose) cat("Plotting natural mortality info\n");
    mdfr<-getMDFR.NatMort(tcsams,rsims,verbose);
    p<-plotMDFR.Bars(mdfr,x='m',agg.formula=NULL,faceting='pc~x',
                     fill='model',xlab='maturity',ylab='natural mortality');
    if (showPlot||!is.null(pdf)) print(p);
    plots$M_cxm<-p;
    
    #mean growth increments
    if (verbose) cat("Plotting mean growth increments\n");
    mdfr<-getMDFR.MeanGrowthIncrements(tcsams,rsims,verbose);
    ##mdfr<-mdfr[,c('pc','x','z','val','model','modeltype')];
    p<-plotMDFR.XY(mdfr,x='z',value.var='val',faceting='pc~x',
                   plotABline=TRUE,
                   xlab='pre-molt size (mm CW)',ylab='post-molt size (mm CW)',units="",
                   shape='model',guideTitleShape='',
                   colour='model',guideTitleColour='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$mnZAM_cz<-p;
    
    #growth transition matrices
    if (verbose) cat("Plotting growth transition matrices\n");
    mdfr<-getMDFR.GrowthTansitionMatrices(tcsams,rsims,verbose);
    p<-plotMDFR.Bubbles(mdfr,x='zp',y='z',faceting='model+pc~x',
                        xlab='pre-molt size (mm CW)',ylab='post-molt size (mm CW)',units="",
                        colour='.',guideTitleColor='',useColourGradient=TRUE,alpha=0.5);
    if (showPlot||!is.null(pdf)) print(p);
    plots$T_czz<-p;
    
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
        mdfr<-mdfr[,c('pc','x','z','val','model','modeltype')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR('mp/prMolt2Mat_cxz',NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    p<-plotMDFR.XY(mdfr,x='z',agg.formula='pc+x+model+z',faceting='pc~x',
                   colour='model',guideTitleColour='',
                   shape='model',guideTitleShape='',
                   xlab='size (mm CW)',ylab='pr(molt-to-maturity)');
    if (showPlot||!is.null(pdf)) print(p);
    plots$prMolt2Mat_cz<-p;
        
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
        mdfr<-mdfr[,c('pc','z','val','model','modeltype')];
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR(path,NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    p<-plotMDFR.XY(mdfr,x='z',agg.formula='pc+x+model+z',faceting='pc~.',
                   xlab='size (mm CW)',ylab='Recruitment size distribution',units='millions',lnscale=FALSE,
                   colour='model',guideTitleColour='',
                   shape='model',guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$R_cz<-p;
    
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
    }
    if (!is.null(rsims)){
        mdfrp<-getMDFR(path,NULL,rsims);
        ums<-as.character(unique(mdfrp$model))
        for (um in ums){
            idx<-(mdfrp$model==um);
            mdfrp$pc[idx]<-reformatTimeBlocks(mdfrp$pc[idx],rsims[[um]]$mc$dims);
        }
        mdfr<-rbind(mdfr,mdfrp);
    }
    p<-plotMDFR.Bars(mdfr,x='pc',agg.formula=NULL,faceting=NULL,
                     fill='model',xlab='year block',ylab='sex ratio');
    if (showPlot||!is.null(pdf)) print(p);
    plots$Rx_c<-p;
    
    #initial size distribution
    if (verbose) cat("Plotting initial size distribution\n");
    path<-'mr/iN_xmsz';
    mdfr<-getMDFR(path,tcsams,rsims);
    mdfr<-removeImmOS(mdfr);
    p<-plotMDFR.XY(mdfr,x='z',agg.formula='model+x+m+s+z',faceting='m+s~x',
                          xlab='size (mm CW)',ylab='Initial size distribution',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColour='Model\nCase',
                          shape=NULL,guideTitleShape='');
    if (showPlot||!is.null(pdf)) print(p);
    plots$iN_xmsz<-p;
        
    if (verbose) cat("Done compareModels.PopProcesses. \n");
    return(invisible(plots))
}