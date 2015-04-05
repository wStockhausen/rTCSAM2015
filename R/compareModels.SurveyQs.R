#'
#'@title Compare survey Qs from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare survey Qs from TCSAM2015 and rsimTCSAM model runs.
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
compareModels.SurveyQs<-function(tcsam=NULL,
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
    
    #survey Qs
    path<-'mp/S_list/Q_vyxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr$v<-gsub("_"," ",mdfr$v,fixed=TRUE);#replace '_'s in survey names with spaces
    
    uniqVs<-unique(mdfr$v);#survey names
    
    for (uV in uniqVs){
        mdfrp<-mdfr[mdfr$v==uV,];#select results for survey uV
        #plot fully-selected rates
        ddfr<-dcast(mdfrp,modeltype+model+y+x~.,fun.aggregate=max,na.rm=FALSE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                       title=uV,xlab='year',ylab='fully-selected Q',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='model',
                       shape='modeltype',guideTitleShape='model type');
        if (showPlot) print(p);
        plots[[uV]]$maxQ_yx<-p;
        
        #plot average (across msz) rates
        ddfr<-dcast(mdfrp,modeltype+model+y+x~.,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='x~.',
                       title=uV,xlab='year',ylab='size-averaged Q',units='',lnscale=FALSE,
                       colour='model',guideTitleColor='model',
                       shape='modeltype',guideTitleShape='model type');
        if (showPlot) print(p);
        plots[[uV]]$avgQ_yx<-p;
    }#uniqVs
    
    return(invisible(plots))
}