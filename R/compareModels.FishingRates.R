#'
#'@title Compare fishing rates from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare fishing rates from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'@param verbose - flag (T/F) to print debug info
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModels.FishingRates<-function(tcsam=NULL,
                                      rsim=NULL,
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
    
    plots<-list();
    
    #capture rates
    if (verbose) cat("getting capture rates\n");
    path<-'mp/F_list/cpF_fyxmsz';
    cmdfr<-getMDFR(path,tcsam,rsim);
    cmdfr$type<-'capture';
    
    #total mortality rates
    if (verbose) cat("getting total mortality rates\n");
    path<-'mp/F_list/tmF_fyxmsz';
    tmdfr<-getMDFR(path,tcsam,rsim);
    tmdfr$type<-'total mortality';
    
    #retention mortality rates
    if (verbose) cat("getting retention mortality rates\n");
    path<-'mp/F_list/rmF_fyxmsz';
    rmdfr<-getMDFR(path,tcsam,rsim);
    rmdfr$type<-'retention mortality';
    
    #discard mortality rates
    if (verbose) cat("getting discard mortality rates\n");
    path<-'mp/F_list/dmF_fyxmsz';
    dmdfr<-getMDFR(path,tcsam,rsim);
    dmdfr$type<-'discard mortality';
    
    mdfr<-rbind(cmdfr,tmdfr,rmdfr,dmdfr);#concatenate types in one dataframe
    
    uniqFs<-unique(mdfr$f);#fishery names
    uniqFs<-uniqFs[uniqFs!=''];#drop placeholder names
    
    for (uF in uniqFs){
        if (verbose) cat("Plotting results for ",uF,"\n");
        mdfrp<-mdfr[mdfr$f==uF,];#select fishery results
        #plot fully-selected rates
        if (verbose) cat("Plotting fully-selected rates\n");
        ddfr<-reshape2::dcast(mdfrp,modeltype+model+type+y+x~.,fun.aggregate=max,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='type~x',
                       title=uF,xlab='year',ylab='fully-selected fishing rate',units='',lnscale=FALSE,
                       colour='model',guideTitleColour='model',
                       shape='modeltype',guideTitleShape='model type',
                       showPlot=showPlot);
        plots[[uF]]$maxF_yx<-p;
        
        #plot average (across msz) rates
        if (verbose) cat("Plotting average rates\n");
        ddfr<-reshape2::dcast(mdfrp,modeltype+model+type+y+x~.,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='type~x',
                       title=uF,xlab='year',ylab='size-averaged fishing rate',units='',lnscale=FALSE,
                       colour='model',guideTitleColour='model',
                       shape='modeltype',guideTitleShape='model type',
                       showPlot=showPlot);
        plots[[uF]]$avgF_yx<-p;
    }#uniqFs
        
    #-------------
    #total fishing mortality rates (across fisheries)
    if (verbose) cat("Plotting total fishing mortality rates\n");
    path<-'mp/F_list/tmF_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    
    #plot fully-selected rates
    ddfr<-reshape2::dcast(mdfr,modeltype+model+y+x~.,fun.aggregate=max,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                   title='all fisheries',xlab='year',ylab='fully-selected total fishing mortality rate',units='',lnscale=FALSE,
                   colour='model',guideTitleColour='model',
                   shape='modeltype',guideTitleShape='model type',
                   showPlot=showPlot);
    plots$maxF_yx<-p;
    
    #plot average (across msz) rates
    ddfr<-reshape2::dcast(mdfr,modeltype+model+y+x~.,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                          title='all fisheries',xlab='year',ylab='size-averaged total fishing mortality rate',units='',lnscale=FALSE,
                          colour='model',guideTitleColour='model',
                          shape='modeltype',guideTitleShape='model type',
                   showPlot=showPlot);
    plots$avgF_yx<-p;
    
    return(invisible(plots))
}