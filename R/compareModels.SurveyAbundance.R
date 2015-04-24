#'
#'@title Compare estimated survey abundances from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare estimated survey catches abundances from TCSAM2015 and rsimTCSAM model runs.
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
compareModels.SurveyAbundance<-function(tcsam=NULL,
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
    
    #abundance
    path<-'mr/S_list/N_vyxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr$v<-gsub("_"," ",mdfr$v,fixed=TRUE);#replace '_'s in survey names with spaces
    mdfr<-removeImmOS(mdfr);
    
    uniqVs<-unique(mdfr$v);#survey names
    
    for (uV in uniqVs){
        #plot totals across z
        mdfrp<-mdfr[mdfr$v==uV,];#select results for survey uV        
        ddfr<-dcast(mdfrp,modeltype+model+y+x+m+s~.,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='m+s~x',
                       title=uV,xlab='year',ylab='survey abundance',units='millions',lnscale=FALSE,
                       colour='model',guideTitleColor='model',
                       shape='modeltype',guideTitleShape='model type');
        if (showPlot) print(p);
        plots[[uV]]$N_yx<-p;
    }#uniqFs
    
    return(invisible(plots))
}