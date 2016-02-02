#'
#'@title Plot size comps from a single year (possibly for multiple models).
#'
#'@description Function to plot size comps from a single year (possibly for multiple models).
#'
#'@param n_xmsz - array (or list of arrays) dimensioned xmsz
#'@param res - TCSAM2015 results object (or list of such)
#'@param component - object name in res from which to extract n_xmsyz
#'@param year - year to extract size comps from component in res
#'@param mdl - model name (if single model is given)
#'@param title - title for plot
#'@param showPlot - flag to show plot immediately
#'
#'@return ggplot2 object
#'
#'@import ggplot2
#'
#'@export
#'
plotSizeCompsGG.Single<-function(n_xmsz=NULL,
                                 res=NULL,
                                 component='pop.quants',
                                 year=NULL,
                                 mdl='',
                                 title='',
                                 showPlot=TRUE){
    
    oneModel<-FALSE;
    if (!is.null(n_xmsz)){
        #size comps come in as array(s)
        if (is.array(n_xmsz)){
            mdfr<-reshape2::melt(n_xmsz,value.name='val');
            mdfr$model<-mdl;
            oneModel<-TRUE;
        } else if (is.list(n_xmsz)) {
            #n_xmsz is a list of array objects with models as names 
            mdls<-names(n_xmsz);
            mdfr<-NULL;
            for (mdl in mdls){
                mdfrp<-reshape2::melt(n_xmsz[[mdl]],value.name='val');
                mdfrp$model<-mdl;
                mdfr<-rbind(mdfr,mdfrp);
            }
        } else {
            cat('Invalid specification for n_xmsz in plotSingleSizeCompGG\n');
            cat('n_xmsz must be either an array or a list of arrays\n');
            cat("Aborting...\n")
            stop();
        }
    } else if (!is.null(res)){
        if ((class(res)=='tcsam2015')){
            if (!is.null(year)) {
                yr<-as.character(year);
            } else {
                yr<-as.character(res$mc$mnYr);
            }
            n_xmsz <- res[[component]]$n.xmsyz[,,,yr,];
            mdfr<-reshape2::melt(n_xmsz,value.name='val');
            mdfr$model<-res$mc$configName;
            oneModel<-TRUE;
        } else if (is.list(res)){
            #res is a list of tcsam2015 model results objects
            mdls<-names(res);
            mdfr<-NULL;
            for (mdl in mdls){
                if (!is.null(year)) {
                    yr<-as.character(year);
                } else {
                    yr<-as.character(res[[mdl]]$mc$mnYr);
                }
                n_xmsz <- (res[[mdl]])[[component]]$n.xmsyz[,,,yr,];
                mdfrp<-reshape2::melt(n_xmsz,value.name='val');
                mdfrp$model<-mdl;
                mdfr<-rbind(mdfr,mdfrp);
            }
        } else {
            cat('Invalid specification for res in plotSingleSizeCompGG\n');
            cat('res must be either a tcsam2015 results object or a list of such\n');
            cat("Aborting...\n")
            stop();
        }
    }
            
    pl <- ggplot(aes(x=size,y=val,fill=shell_condition),data=mdfr);
    pl <- pl + geom_bar(alpha=0.5,stat='identity',position='dodge');
    pl <- pl + labs(x='size (mm)',y='abundance');
    if (oneModel){
        #plotting one model
        pl <- pl + guides(fill=guide_legend(''));
        pl <- pl + facet_grid(maturity~sex);
    } else {
        #plotting multiple models
        pl <- pl + guides(fill=guide_legend('model'))
        pl <- pl + facet_grid(maturity+shell_condition~sex);
    }
    if (title!='') pl <- pl + ggtitle(title);
    if (showPlot) print(pl);
    
    return(invisible(pl))
}