#'
#'@title Compare population-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare population-related quantities from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModels.PopQuants<-function(tcsam=NULL,
                                  rsim=NULL,
                                  showPlot=TRUE,
                                  pdf=NULL,
                                  width=8,
                                  height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=out.pdf,width=width,height=height);
        on.exit(dev.close())
    }
    
    plots<-list();
    
    #natural mortality
    path<-'mp/M_cxm';
    mdfr<-getMDFR(path,tcsam,rsim);
    if (!is.null(tcsam)){
        pgi<-tcsam$mpi$nm$pgi;
        nPCs<-length(pgi$pcs);
        mdfr$y<-'';
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$modeltype=='tcsam');
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
        }
    }
    p<-plotMDFR.Bars(mdfr,x='x',agg.formula=NULL,faceting='y~m',
                     fill='model',xlab='sex',ylab='natural mortality');
    if (showPlot) print(p);
    plots$M_cxm<-p;
    
    #growth
    path<-'mp/prGr_czz';
    mdfr<-getMDFR(path,tcsam,rsim);
#         mdfr$z <-as.numeric(mdfr$z);
#         mdfr$zp<-as.numeric(mdfr$zp);
    if (!is.null(tcsam)){
        pgi<-tcsam$mpi$grw$pgi;
        nPCs<-length(pgi$pcs);
        mdfr$y<-'';
        mdfr$x<-'';
        for (pc in 1:nPCs){
            idx<-mdfr$pc==pc;
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
            mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
        }
    }
    p<-plotMDFR.Bubbles(mdfr,x='zp',y='z',faceting='model+y~x',
                        xlab='pre-molt size (mm CW)',ylab='post-molt size (mm CW)',units="",
                        colour='.',guideTitleColor='',useColourGradient=TRUE,alpha=0.5);
    if (showPlot) print(p);
    plots$prGr_czz<-p;
    
    #molt-to-maturity
    path<-'mp/prMolt2Mat_cz';
    mdfr<-getMDFR(path,tcsam,rsim);
#         mdfr$z <-as.numeric(mdfr$z);
    if (!is.null(tcsam)){
        pgi<-tcsam$mpi$mat$pgi;
        nPCs<-length(pgi$pcs);
        mdfr$y<-'';
        mdfr$x<-'';
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$modeltype=='tcsam');
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
            mdfr$x[idx]<-tolower(pgi$pcs[[pc]]$SEX);
        }
    }
    p<-plotMDFR.Bars(mdfr,x='z',agg.formula=NULL,faceting='y~x',
                     fill='model',xlab='size (mm CW)',ylab='pr(molt-to-maturity)');
    if (showPlot) print(p);
    plots$prMolt2Mat_cz<-p;
        
    #recruitment size distribution
    path<-'mp/R_list/R_cz';
    mdfr<-getMDFR(path,tcsam,rsim);
    if (!is.null(tcsam)){
        pgi<-tcsam$mpi$rec$pgi;
        nPCs<-length(pgi$pcs);
        mdfr$y<-'';
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$modeltype=='tcsam');
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
        }
    }
    p<-plotMDFR.XY(mdfr,x='z',agg.formula=NULL,faceting='y~.',
                   xlab='size (mm CW)',ylab='Recruitment size distribution',units='millions',lnscale=FALSE,
                   colour='model',guideTitleColor='',
                   shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$R_cz<-p;
    
    #recruitment sex ratio
    path<-'mp/R_list/Rx_c';
    mdfr<-getMDFR(path,tcsam,rsim);
    if (!is.null(tcsam)){
        pgi<-tcsam$mpi$rec$pgi;
        nPCs<-length(pgi$pcs);
        mdfr$y<-'';
        for (pc in 1:nPCs){
            idx<-(mdfr$pc==pc)&(mdfr$modeltype=='tcsam');
            mdfr$y[idx]<-pgi$pcs[[pc]]$YEAR_BLOCK;
        }
    }
    p<-plotMDFR.Bars(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                     fill='model',xlab='year block',ylab='sex ratio');
    if (showPlot) print(p);
    plots$Rx_c<-p;
    
    #initial size distribution
    path<-'mr/iN_xmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='z',agg.formula='model+x+m+s+z',faceting='m+s~x',
                          xlab='size (mm CW)',ylab='Initial size distribution',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$iN_xmsz<-p;
        
    #recruitment
    path<-'mp/R_list/R_y';
    mdfr<-getMDFR(path,tcsam,rsim);
#     mdfr$y<-as.numeric(mdfr$y);
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                   colour='model',guideTitleColor='',
                   shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$R_y<-p;
    p<-plotMDFR.XY(mdfr,x='y',agg.formula=NULL,faceting=NULL,
                   xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                   colour='model',guideTitleColor='',
                   shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$lnR_y<-p;
    
    #Mature biomass
    path<-'mr/P_list/MB_yx';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',agg.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$MB_yx<-p;
    
    #Population biomass
    path<-'mr/P_list/B_yxms';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',agg.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Population Biomass',units="1000's t",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$B_yx<-p;
    
    #Population abundance
    path<-'mr/P_list/N_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',agg.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Population Abundance',units="millions",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    if (showPlot) print(p);
    plots$N_yx<-p;
    
    #Population abundance-at-size
    path<-'mr/P_list/N_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.Bubbles(mdfr,x='y',y='z',
                        agg.formula='model+y+x+z',faceting='model~x',
                        xlab='year',ylab='size (mm CW)',units="millions",
                        colour='.',guideTitleColor='',useColourGradient=TRUE,alpha=0.5);
    if (showPlot) print(p);
    plots$N_yxmsz<-p;
        
    return(invisible(plots))
}