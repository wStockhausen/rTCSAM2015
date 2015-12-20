#'
#'@title Compare selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@description Function to compare selectivity functions from TCSAM2015 and rsimTCSAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param years - list, by survey name, of years to plot
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
compareModels.SurveySelFcns<-function(tcsam=NULL,
                                      rsim=NULL,
                                      years=list(`NMFS Trawl Survey`=2014),
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
    path<-'mp/S_list/sel_vyxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr$v<-gsub("_"," ",mdfr$v,fixed=TRUE);#replace '_'s in survey names with spaces
    
    uniqVs<-unique(mdfr$v);#survey names
    names(years)<-tolower(names(years));#convert names for years to lower case
    
    castform<-"modeltype+model+v+y+z~.";
    if (!is.null(cast)&&(cast!='')) castform<-paste("modeltype+model+v+y+",cast,"+z~.",sep='');
    
    plots<-list();
    for (uV in uniqVs){
        cat('creating selectivity functions plot for survey ',uV,'\n')
        mdfrp<-mdfr[(mdfr$v==uV)&(mdfr$y %in% years[[tolower(uV)]]),];#select results for survey uV        
        if (nrow(mdfrp)>0){
            ddfr<-dcast(mdfrp,castform,fun.aggregate=mean,na.rm=TRUE,value.var='val',drop=TRUE)
            ddfr[['.']]<-ifelse(ddfr[['.']]==0,NA,ddfr[['.']]);
            p<-plotMDFR.XY(ddfr,x='z',value.var='.',
                           agg.formula=NULL,faceting=faceting,
                           xlab='size (mm CW)',ylab='Selectivity',units='',lnscale=FALSE,
                           title=uV,
                           colour='model',guideTitleColor='',
                           shape='model',guideTitleShape='');
            if (showPlot) print(p);
            plots[[uV]]$selfcn<-p;
        }
    }#uniqVs
    return(invisible(plots))
}
