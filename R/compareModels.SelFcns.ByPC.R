#'
#'@title Compare selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model report object, or named list of such
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
compareModels.SelFcns.ByPC<-function(tcsam=NULL,
                                     rsim=NULL,
                                     showPlot=TRUE,
                                     pdf=NULL,
                                     width=8,
                                     height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close())
    }
        
    #selectivity/retention
    path<-'mp/sel_cz';
    mdfr<-getMDFR(path,tcsam,rsim);
    
    #if only tcsam models are present
    if (!is.null(tcsam)&&is.null(rsim)){
        plots<-list();
        if (class(tcsam)[1]=='tcsam2015.rep'){
            #need to wrap it in a list
            tcsam<-list(tcsam=tcsam);
        }
        
        #process list to extract selectivity function labels
        mdfr$y<-'';
        mdfr$x<-'';
        mdfr$prctype<-'';
        mdfr$prcname<-'';
        mdfr$fcntype<-'';
        for (mdl in names(tcsam)){
            sel.pgi<-tcsam[[mdl]]$mpi$srv$pgi;
            #surveys
            srv.pgi<-tcsam[[mdl]]$mpi$srv$pgi;
            srv.nPCs<-length(srv.pgi$pcs)-1;
            for (pc in 1:srv.nPCs){
                pci<-srv.pgi$pcs[[pc]];
                n<-tcsam[[mdl]]$mc$dims$v$nms[as.numeric(pci$SURVEY)];
                y<-pci$YEAR_BLOCK;
                x<-tolower(pci$SEX);
                idx.SelFcn<-pci$ids.PC['idx.SelFcn']
                #idx<-(mdfr$pc==idx.SelFcn)&(mdfr$modeltype=='tcsam');
                idx<-(mdfr$pc==idx.SelFcn)&(mdfr$model==mdl);
                for (i in 1:length(idx)){
                    if (idx[i]){
                        mdfr$y[i]<-appendString(mdfr$y[i],y,sep='/');
                        mdfr$x[i]<-appendString(mdfr$x[i],x,sep='/');
                        mdfr$prctype[i]<-appendString(mdfr$prctype[i],'survey',sep='/');
                        mdfr$prcname[i]<-appendString(mdfr$prcname[i],n,sep='\n');
                        mdfr$fcntype[i]<-appendString(mdfr$fcntype[i],'selectivity',sep='/');
                    }
                }
            }#surveys
            
            #fisheries
            fsh.pgi<-tcsam[[mdl]]$mpi$fsh$pgi;
            fsh.nPCs<-length(fsh.pgi$pcs)-1;
            for (pc in 1:fsh.nPCs){
                pci<-fsh.pgi$pcs[[pc]];
                n<-tcsam[[mdl]]$mc$dims$f$nms[as.numeric(pci$FISHERY)];
                y<-pci$YEAR_BLOCK;
                x<-tolower(pci$SEX);
                idx.SelFcn<-pci$ids.PC['idx.SelFcn']
                #idx<-(mdfr$pc==idx.SelFcn)&(mdfr$modeltype=='tcsam');
                idx<-(mdfr$pc==idx.SelFcn)&(mdfr$model==mdl);
                for (i in 1:length(idx)){
                    if (idx[i]){
                        mdfr$y[i]<-appendString(mdfr$y[i],y,sep='/');
                        mdfr$x[i]<-appendString(mdfr$x[i],x,sep='/');
                        mdfr$prctype[i]<-appendString(mdfr$prctype[i],'fishery',sep='/');
                        mdfr$prcname[i]<-appendString(mdfr$prcname[i],n,sep='\n');
                        mdfr$fcntype[i]<-appendString(mdfr$fcntype[i],'selectivity',sep='/');
                    }
                }
                idx.RetFcn<-pci$ids.PC['idx.RetFcn'];
                if (idx.RetFcn>0){
                    idx<-(mdfr$pc==idx.RetFcn);
                    for (i in 1:length(idx)){
                        if (idx[i]){
                            mdfr$y[i]<-appendString(mdfr$y[i],y,sep='/');
                            mdfr$x[i]<-appendString(mdfr$x[i],x,sep='/');
                            mdfr$prctype[i]<-appendString(mdfr$prctype[i],'fishery',sep='/');
                            mdfr$prcname[i]<-appendString(mdfr$prcname[i],n,sep='\n');
                            mdfr$fcntype[i]<-appendString(mdfr$fcntype[i],'retention',sep='/');
                        }
                    }
                }
            }#fisheries
        }#loop over tcsam models
    
        #plot surveys
        idx<-(mdfr$prctype=='survey');
        p<-plotMDFR.XY(mdfr[idx,],x='z',
                       agg.formula=NULL,faceting='prcname+y~x',
                       xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='',
                       shape='model',guideTitleShape='');
        if (showPlot) print(p);
        plots$surveys<-p;
        
        #plot fisheries selectivity
        fisheries<-list();
        idx<-(mdfr$prctype=='fishery')&(mdfr$fcntype=='selectivity');
        p<-plotMDFR.XY(mdfr[idx,],x='z',
                       agg.formula=NULL,faceting='prcname+y~x',
                       xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='',
                       shape='model',guideTitleShape='');
        if (showPlot) print(p);
        fisheries$sel<-p;
        #plot fisheries selectivity
        idx<-(mdfr$prctype=='fishery')&(mdfr$fcntype=='retention');
        p<-plotMDFR.XY(mdfr[idx,],x='z',
                       agg.formula=NULL,faceting='prcname+y~x',
                       xlab='size (mm CW)',ylab='Retention',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='',
                       shape='model',guideTitleShape='');
        if (showPlot) print(p);
        fisheries$ret<-p;
        plots$fisheries<-fisheries;
            
        return(invisible(plots))
    }#tcsam but no rsim
    
    #only rsim models
    if (is.null(tcsam)&&(!is.null(rsim))){
        if (class(rsim)!='list'){
            rsim<-list(rsim=rsim)
        }
        
        #process list to extract selectivity function labels
        mdfr$selfcns<-'';
        for (mdl in names(rsim)){
            d<-rsim[[mdl]]$mc$dims;
            idx<-(mdfr$model==mdl);
            mdfr$selfcns[idx]<-d$selfcns$lbls[as.numeric(mdfr$pc[idx])];
        }#models
        #plot
        p<-plotMDFR.XY(mdfr,x='z',
                       agg.formula=NULL,faceting='selfcns~.',
                       xlab='size (mm CW)',ylab='Selectivity/Retention',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='',
                       shape='model',guideTitleShape='');
        if (showPlot) print(p);
        return(invisible(p))
    }#rsim but no tcsam
    
    #both tcsam and rsim models, so plot using pc's, not labels 
    npc<-5;
    nPCs<-length(unique(mdfr$pc));
    npgs<-ceiling(nPCs/npc);
    p<-list();
    for (pg in 1:npgs){
        pcs<-1:npc+(pg-1)*npc;
        mdfrp<-mdfr[mdfr$pc %in% pcs,];
        mdfrp$pc<-formatZeros(mdfrp$pc,width=2);
        p[[pg]]<-plotMDFR.XY(mdfrp,x='z',
                             agg.formula=NULL,faceting='pc~.',
                             xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                             colour='model',guideTitleColour='',
                             shape='model',guideTitleShape='');
    }
    if (showPlot) print(p);
        
    return(invisible(p))
}