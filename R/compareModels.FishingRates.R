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
#'
#'@return list of ggplot2 objects
#'
#'@import reshape2
#'
#'@export
#'
compareModels.FishingRates<-function(tcsam=NULL,
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
    
    plots<-list();
    
    #capture rates
    path<-'mp/F_list/cpF_fyxmsz';
    cmdfr<-getMDFR(path,tcsam,rsim);
    cmdfr$type<-'capture';
    
    #total mortality rates
    path<-'mp/F_list/tmF_fyxmsz';
    tmdfr<-getMDFR(path,tcsam,rsim);
    tmdfr$type<-'total mortality';
    
    #retention mortality rates
    path<-'mp/F_list/rmF_fyxmsz';
    rmdfr<-getMDFR(path,tcsam,rsim);
    rmdfr$type<-'retention mortality';
    
    #discard mortality rates
    path<-'mp/F_list/dmF_fyxmsz';
    dmdfr<-getMDFR(path,tcsam,rsim);
    dmdfr$type<-'discard mortality';
    
    mdfr<-rbind(cmdfr,tmdfr,rmdfr,dmdfr);#concatenate types in one dataframe
    
    uniqFs<-unique(mdfr$f);#fishery names
    uniqFs<-uniqFs[uniqFs!=''];#drop placeholder names
    
    for (uF in uniqFs){
        mdfrp<-mdfr[mdfr$f==uF,];#select fishery results
        #plot fully-selected rates
        ddfr<-dcast(mdfrp,modeltype+model+type+y+x~.,fun.aggregate=max,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='type~x',
                       title=uF,xlab='year',ylab='fully-selected fishing rate',units='',lnscale=FALSE,
                       colour='model',guideTitleColour='model',
                       shape='modeltype',guideTitleShape='model type');
        if (showPlot) print(p);
        plots[[uF]]$maxF_yx<-p;
        
        #plot average (across msz) rates
        ddfr<-dcast(mdfrp,modeltype+model+type+y+x~.,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='type~x',
                       title=uF,xlab='year',ylab='size-averaged fishing rate',units='',lnscale=FALSE,
                       colour='model',guideTitleColour='model',
                       shape='modeltype',guideTitleShape='model type');
        if (showPlot) print(p);
        plots[[uF]]$avgF_yx<-p;
    }#uniqFs
        
    #-------------
    #total fishing mortality rates (across fisheries)
    path<-'mp/F_list/tmF_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    
    #plot fully-selected rates
    ddfr<-dcast(mdfr,modeltype+model+y+x~.,fun.aggregate=max,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                   title='all fisheries',xlab='year',ylab='fully-selected total fishing mortality rate',units='',lnscale=FALSE,
                   colour='model',guideTitleColour='model',
                   shape='modeltype',guideTitleShape='model type');
    if (showPlot) print(p);
    plots$maxF_yx<-p;
    
    #plot average (across msz) rates
    ddfr<-dcast(mdfr,modeltype+model+y+x~.,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
    ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
    p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                          title='all fisheries',xlab='year',ylab='size-averaged total fishing mortality rate',units='',lnscale=FALSE,
                          colour='model',guideTitleColour='model',
                          shape='modeltype',guideTitleShape='model type');
    if (showPlot) print(p);
    plots$avgF_yx<-p;
    
    return(invisible(plots))
}