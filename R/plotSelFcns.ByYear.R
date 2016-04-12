#'
#'@title Compare selectivity functions from TCSAM2015 and rsimTCSAM model runs by year
#'
#'@description Function to compare selectivity functions from TCSAM2015 and rsimTCSAM model runs by year.
#'
#'@param repObj - single TCSAM2015 model report object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
plotSelFcns.ByYear<-function(repObj=NULL,
                             nrow=3,
                             ncol=4,
                             showPlot=TRUE,
                             pdf=NULL,
                             width=8,
                             height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close())
    }
        
    #fishery selectivity functions
    path<-'mp/F_list/sel_fyxmsz';
    mdfr<-getMDFR(path,repObj,NULL);
    zdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~z,fun.aggregate=mean,value.var='val');
    mdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','f','x','y'),variable.name='z',value.name='val');
    zdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~z,fun.aggregate=sum,value.var='val');
    tdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~.,fun.aggregate=sum,value.var='val');
    idx<-which(tdfr$`.`!=0);
    zdfr<-zdfr[idx,];
    sdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','f','x','y'),variable.name='z',value.name='val');
    sdfr$z<-as.numeric(as.character(sdfr$z));
    sdfr$type<-'selectivity';
    
    #fishery retention functions
    path<-'mp/F_list/ret_fyxmsz';
    mdfr<-getMDFR(path,repObj,NULL);
    zdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~z,fun.aggregate=mean,value.var='val');
    mdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','f','x','y'),variable.name='z',value.name='val');
    zdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~z,fun.aggregate=sum,value.var='val');
    tdfr<-reshape2::dcast(mdfr,modeltype+model+f+x+y~.,fun.aggregate=sum,value.var='val');
    idx<-which(tdfr$`.`!=0);
    zdfr<-zdfr[idx,];
    rdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','f','x','y'),variable.name='z',value.name='val');
    rdfr$z<-as.numeric(as.character(rdfr$z));
    rdfr$type<-'retention';
    
    fdfr<-rbind(sdfr,rdfr);
    
    #plot fisheries selectivity/retention
    uys<-sort(as.character(unique(fdfr$y)),decreasing=FALSE);
    nys<-length(uys);
    npg<-ceiling(nys/(nrow*ncol));
    fisheries<-list();
    for (pg in 1:npg){
        uysp<-uys[(1+(pg-1)*nrow*ncol):min(pg*nrow*ncol,nys)];
        fdfrp<-fdfr[fdfr$y %in% uysp,];
        p<-plotMDFR.XY(fdfrp,x='z',value.var='val',
                       agg.formula=NULL,facet_wrap='y',ncol=ncol,
                       xlab='size (mm CW)',ylab='Selectivity/Retention',
                       units='',lnscale=FALSE,ylim=c(0,1),
                       colour='f',     guideTitleColour='fishery',
                       linetype='type',guideTitleLineType='type',
                       fill='x',       guideTitleFill='sex',
                       shape='x',      guideTitleShape='sex');
        if (showPlot) print(p);
        fisheries[[pg]]<-p;
    }
    
    #survey selectivity functions
    path<-'mp/S_list/sel_vyxmsz';
    mdfr<-getMDFR(path,repObj,NULL);
    zdfr<-reshape2::dcast(mdfr,modeltype+model+v+x+y~z,fun.aggregate=mean,value.var='val');
    mdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','v','x','y'),variable.name='z',value.name='val');
    zdfr<-reshape2::dcast(mdfr,modeltype+model+v+x+y~z,fun.aggregate=sum,value.var='val');
    tdfr<-reshape2::dcast(mdfr,modeltype+model+v+x+y~.,fun.aggregate=sum,value.var='val');
    idx<-which(tdfr$`.`!=0);
    zdfr<-zdfr[idx,];
    sdfr<-reshape2::melt(zdfr,id.vars=c('modeltype','model','v','x','y'),variable.name='z',value.name='val');
    sdfr$z<-as.numeric(as.character(sdfr$z));
    sdfr$type<-'selectivity';
    
    #plot survey selectivities
    uys<-as.character(unique(sdfr$y));
    nys<-length(uys);
    npg<-ceiling(nys/(nrow*ncol));
    surveys<-list();
    for (pg in 1:npg){
        uysp<-uys[(1+(pg-1)*nrow*ncol):min(pg*nrow*ncol,nys)];
        sdfrp<-sdfr[sdfr$y %in% uysp,];
        p<-plotMDFR.XY(sdfrp,x='z',value.var='val',
                       agg.formula=NULL,facet_wrap='y',ncol=ncol,
                       xlab='size (mm CW)',ylab='Selectivity',
                       units='',lnscale=FALSE,ylim=c(0,1),
                       colour='v',     guideTitleColour='survey',
                       linetype='type',guideTitleLineType='type',
                       fill='x',       guideTitleFill='sex',
                       shape='x',      guideTitleShape='sex');
        if (showPlot) print(p);
        surveys[[pg]]<-p;
    }
    
    return(invisible(list(fisheries=fisheries,surveys=surveys)))
}