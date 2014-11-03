#'
#'@title Get symbology info for plotting purposes
#'
#'@description Get a list with the TCSAM2014 standard plot symbology
#'
#'@param res - results list from TCSAM2014 model run
#'
#'@return list with elements 'col','pch','lty', and 'leg.info'. \cr 
#'leg.info is in turn a list, with elements \cr
#''col','txt','txt.col','pch', and 'lty'.
#'
#'
#'@export
#'
getPlotSymbology<-function(res,
                           col.SX=c("green","blue"),
                           pch.MS=c(21,22),
                           lty.SC=c(2,1)){
    nSXs<-length(res$mc$SXs);
    nMSs<-length(res$mc$MSs);
    nSCs<-length(res$mc$SCs);
    nFCs<-nSXs*nMSs*nSCs;
    
    leg.txt<-c(res$mc$SXs,res$mc$MSs,res$mc$SCs);
    leg.txt.col<-vector(mode="character",length=nFCs);
    leg.pch<-vector(mode="integer",length=nFCs)+NA;
    leg.lty<-vector(mode="integer",length=nFCs)+NA;
    
    leg.txt.col[1:nFCs]<-"black";
    leg.txt.col[1:nSXs]<-col.SX;
    leg.pch[(nSXs+1):(nSXs+nMSs)]<-pch.MS;
    leg.lty[(nSXs+nMSs+1):nFCs]<-lty.SC;
    
    names(col.SX)<-res$mc$SXs;
    names(pch.MS)<-res$mc$MSs;
    names(lty.SC)<-res$mc$SCs;
    
    return(list(col=col.SX,pch=pch.MS,lty=lty.SC,
                leg.info=list(col="black",txt=leg.txt,txt.col=leg.txt.col,pch=leg.pch,lty=leg.lty)));
}