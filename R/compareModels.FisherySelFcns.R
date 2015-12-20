#'
#'@title Compare fishery selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare fishery selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param years - list, by fishery name, of years to plot
#'@param cast - casting formula for excluding x,m,s factor levels from an average-at-size across unspecified factors
#'@param faceting - faceting formula involving y and factors in the cast
#'@param showPlot - flag to show/print plots immediately
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@export
#'
compareModels.FisherySelFcns<-function(tcsam=NULL,
                                      rsim=NULL,
                                      years=list(TCF=2013),
                                      cast='x',
                                      faceting='y~x',
                                      showPlot=TRUE,
                                      pdf=NULL,
                                      width=14,
                                      height=8){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=pdf,width=width,height=height);
        on.exit(dev.close());
    }
        
    #selectivity/retention
    path<-'mp/F_list/sel_fyxmsz';
    sdfr<-getMDFR(path,tcsam,rsim);
    sdfr$type<-'selectivity';
    path<-'mp/F_list/ret_fyxmsz';
    rdfr<-getMDFR(path,tcsam,rsim);
    rdfr$type<-'retention';
    
    mdfr<-rbind(sdfr,rdfr);
    mdfr$v<-gsub("_"," ",mdfr$v,fixed=TRUE);#replace '_'s in fishery names with spaces
    
    uniqFs<-unique(mdfr$f);#fishery names
    names(years)<-tolower(names(years));#convert names for years to lower case
    
    castform<-"modeltype+model+f+type+y+z~.";
    if (!is.null(cast)&&(cast!='')) castform<-paste("modeltype+model+f+type+y+",cast,"+z~.",sep='');
    
    plots<-list();
    for (uF in uniqFs){
        mdfrp<-mdfr[(mdfr$f==uF)&(mdfr$y %in% years[[tolower(uF)]]),];#select results for fishery F      
        if (nrow(mdfrp)>0){
            ddfr<-dcast(mdfrp,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
            ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
            p<-plotMDFR.XY(ddfr,x='z',value.var='.',
                           agg.formula=NULL,faceting=faceting,
                           xlab='size (mm CW)',ylab='Selectivity/Retention',units='',lnscale=FALSE,
                           title=uF,
                           colour='model',guideTitleColor='',
                           linetype='type',guideTitleLinetype='function type',
                           shape='modeltype',guideTitleShape='');
            if (showPlot) print(p);
            plots[[uF]]$selfcn<-p;
        }
    }#uniqFs
    return(invisible(plots))
}
