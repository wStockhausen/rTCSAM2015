#'
#'@title Compare TCSAM2015 and rsimTCAM model runs.
#'
#'@description Function to compare TCSAM2015 and rsimTCAM model runs.
#'
#'@param tcsam - single TCSAM2015 model results object, or named list of such
#'@param rsim - single rsimTCSAM results object, or named list of such
#'@param pdf - name of pdf file to record plot output to
#'@param width - pdf page width (in inches)
#'@param height - pdf page width (in inches)
#'
#'@return list of ggplot2 objects
#'
#'@import ggplot2
#'
#'@export
#'
compareModels<-function(tcsam=NULL,
                        rsim=NULL,
                        pdf="compareModels.pdf",
                        width=8,
                        height=6){
    #set up pdf device, if requested
    if (!is.null(pdf)){
        pdf(file=out.pdf,width=width,height=height);
        on.exit(dev.close())
    }
    
    plts<-list();
    
    #growth
    path<-'mp/T_yxszz';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr<-mdfr[mdfr$y=='1950',];
    p<-plotMDFR.Bubbles(mdfr,x='z',y='zp',
                        cast.formula='model+x+z+zp',faceting='model~x',
                        xlab='pre-molt size (mm CW)',ylab='post-molt size (mm CW)',units="",
                        colour='.',guideTitleColor='',useColourGradient=TRUE,alpha=0.5);
    print(p);
    plts$N_yx<-p;
        
    #recruitment
    path<-'mp/R_list/R_y';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+y',
                          xlab='year',ylab='Recruitment',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$R_y<-p;
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+y',
                          xlab='year',ylab='Recruitment',units='millions',lnscale=TRUE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$lnR_y<-p;
    
    #recruitment size distribution
    path<-'mp/R_list/R_yxz';
    mdfr<-getMDFR(path,tcsam,rsim);
    mdfr<-mdfr[(mdfr$y==mnY)&(mdfr$x==d$x$mns[1]),]
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+z',
                          xlab='size (mm)',ylab='Recruitment size distribution',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$R_z<-p;
    
    #initial size distribution
    path<-'mr/iN_xmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='z',cast.formula='model+x+m+s+z',faceting='m+s~x',
                          xlab='size (mm)',ylab='Initial size distribution',units='millions',lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$iN_xmsz<-p;
    
    #Mature biomass
    path<-'mr/P_list/MB_yx';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Mature Biomass',units="1000's t",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$MB_yx<-p;
    
    #Population biomass
    path<-'mr/P_list/B_yxms';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Population Biomass',units="1000's t",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$B_yx<-p;
    
    #Population abundance
    path<-'mr/P_list/N_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.XY(mdfr,x='y',cast.formula='model+y+x',faceting='x~.',
                          xlab='year',ylab='Population Abundance',units="millions",lnscale=FALSE,
                          colour='model',guideTitleColor='',
                          shape='model',guideTitleShape='');
    print(p);
    plts$N_yx<-p;
    
    #Population abundance-at-size
    path<-'mr/P_list/N_yxmsz';
    mdfr<-getMDFR(path,tcsam,rsim);
    p<-plotMDFR.Bubbles(mdfr,x='y',y='z',
                        cast.formula='model+y+x+z',faceting='model~x',
                        xlab='year',ylab='size (mm CW)',units="millions",
                        colour='.',guideTitleColor='',useColourGradient=TRUE,alpha=0.5);
    print(p);
    plts$N_yx<-p;
        
    return(invisible(plts))
}