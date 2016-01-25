#'
#'@title Compare fishery catches (abundance) from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare fishery catches (abundance) from TCSAM2015 and rsimTCSAM model runs.
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
compareModels.FisheryCatches<-function(tcsam=NULL,
                                       rsim=NULL,
                                       showPlot=TRUE,
                                       pdf=NULL,
                                       width=8,
                                       height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
    
    plots<-list();
    
    #total captured
    path<-'mr/F_list/cpN_fyxmsz';
    cpmdfr<-getMDFR(path,tcsam,rsim);
    cpmdfr$type<-'captured';
    
    #discarded (NOT mortality)
    path<-'mr/F_list/dsN_fyxmsz';
    dsmdfr<-getMDFR(path,tcsam,rsim);
    dsmdfr$type<-'discarded';
    
    #retained mortality (abundance)
    path<-'mr/F_list/rmN_fyxmsz';
    rmmdfr<-getMDFR(path,tcsam,rsim);
    rmmdfr$type<-'retained';
    
    #discard mortality (abundance)
    path<-'mr/F_list/dmN_fyxmsz';
    dmmdfr<-getMDFR(path,tcsam,rsim);
    dmmdfr$type<-'discard mortality';
    
    mdfr<-rbind(cpmdfr,dsmdfr,rmmdfr,dmmdfr);#concatenate types in one dataframe
    mdfr$f<-gsub("_"," ",mdfr$f,fixed=TRUE);#replace '_'s in fishery names with spaces
    
    uniqFs<-unique(mdfr$f);    #fishery names
    uniqFs<-uniqFs[uniqFs!=''];#drop placeholder names
    
    for (uF in uniqFs){
        #plot totals across msz
        mdfrp<-mdfr[mdfr$f==uF,];#select fishery results        
        ddfr<-reshape2::dcast(mdfrp,modeltype+model+type+y+x~.,fun.aggregate=sum,na.rm=TRUE,value.var='val',drop=TRUE)
        ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
        p<-plotMDFR.XY(ddfr,x='y',agg.formula=NULL,faceting='type~x',
                       title=uF,xlab='year',ylab='fishery catch',units='millions',lnscale=FALSE,
                       colour='model',guideTitleColor='model',
                       linetype='modeltype',guideTitleLineType='model type',
                       shape='modeltype',guideTitleShape='model type',
                       showPlot=showPlot);
        plots[[uF]]$N_yx<-p;
    }#uniqFs
    
    return(invisible(plots))
}
