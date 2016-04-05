#'
#'@title Plot an input data timeframe dataframe
#'
#'@description A function to plot an input data timeframe dataframe.
#'
#'@param dfr - dataframe with input data time frames (or a tcsam2015.rep object)
#'@param showPlot - flag (T/F) to print plot
#'
#'@return returns a ggplot2 object
#'
#'@details If dfr is a tcsam2015.rep object, \code{getInputDataTimeframes()} is called on it
#'to get a dataframe in the proper format.
#'
#'@import ggplot2
#'
#'@export
#'
plotInputDataTimeframes<-function(dfr,showPlot=FALSE){
    if (inherits(dfr,'tcsam2015.rep')){
        dfrp<-getInputDataTimeframes(dfr);
    } else {
        dfrp<-dfr;
    }
    
    
    
    uFltTypes<-c("survey","fishery");
    uCatTypes<-c("index.catch","retained.catch","discard.catch","total.catch");
    uDatTypes<-rev(c("abundance","biomass","nAtZ"));
    
    dfrp[["fleet type"]]<-factor(dfrp[["fleet type"]],levels=uFltTypes);
    dfrp[["catch type"]]<-factor(dfrp[["catch type"]],levels=uCatTypes);
    dfrp[["data type"]] <-factor(dfrp[["data type"]],levels=uDatTypes);
    
    uMods<-as.character(unique(dfr$model));
    
    p <- ggplot(dfrp,aes(x=year,y=`data type`,colour=`data type`));
    p <- p + geom_point();
    p <- p + facet_grid(model+`fleet type`+fleet+`catch type`~.,drop=TRUE);
    
    if (showPlot) print(p);
    
    return(p);
    
}