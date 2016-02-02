#'
#'@title Plot a matrix as a color map.
#'
#'@description Function to plot a matrix as a color map.
#'
#'@param mat  - matrix to plot
#'@param zlab - z label
#'@param xlab - label for x axis
#'@param ylab - label for y axis
#'@param zmax - max value for color scale
#'@param zmin - min value for color scale
#'@param title - title for plot
#'@param cex - character expansion factor for plot
#'@param colorscale - color scale to use for plot ('hot', 'cold' or 'coldhot')
#'@param plotsize - 2 element vector giving plot size in pixels (c(W,H))
#'@param showDiagonal - TRUE/FALSE to plot diagonal line
#'
#'@details Uses functions
#'\itemize{
#'  \item wtsUtilities::createColorScale(...)
#'}
#'
#'@export
#'
#----------------------------------------------------------
plotMatrix<-function(mat,
                     zlab="",
                     xlab="",
                     ylab="",
                     zmax=NULL,
                     zmin=0,
                     title=NULL,
                     cex=1,
                     colorscale=c("hot","cold","coldhot"),
                     plotsize=c(970,780),
                     showDiagonal=TRUE){
  nrow<-nrow(mat);
  ncol<-ncol(mat);
  
  ncolors<-100;
  if (is.null(zmax)) {zmax<-max(mat,na.rm=TRUE);}
  if (is.null(zmin)) {zmin<-min(mat,na.rm=TRUE);}
  
  old.par<-par(no.readonly=TRUE);
  on.exit(par(old.par))
  nf<-layout(as.matrix(t(1:2)),widths=c(6,1),heights=1);
  par(mar=c(3,3,3,1),oma=c(2,2,0,0));
  
  cat("zmin = ",zmin,"\n")
  cat("zmax = ",zmax,"\n")
  colorScale<-wtsUtilities::createColorScale(name=colorscale[1]);
  pal<-colorScale((1:ncolors)/ncolors);
  
  image(1:ncol,1:nrow,t(mat),pty='s',col=pal,
        xlab="",ylab="",bty="o",xaxt="n",yaxt="n",zlim=c(zmin,zmax));
  for (i in 2:nrow){ 
    lines(c(1,ncol+1)-0.5,c(i,i)-0.5,lwd=1,col="grey75");
  }
  for (i in 2:ncol){ 
    lines(c(i,i)-0.5,c(1,nrow+1)-0.5,lwd=1,col="grey75");
  }
  
  axis(1,at=1:(ncol+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (!is.null(colnames(mat))){
      axis(1,at=1:ncol,labels=as.numeric(colnames(mat)),cex.axis=cex*0.7,line=0);
  }
  
  axis(2,at=1:(nrow+1)-0.5,labels=FALSE,cex.axis=cex*0.7,line=0,lwd.ticks=0);
  if (!is.null(rownames(mat))){
      axis(2,at=1:nrow,labels=as.numeric(rownames(mat)),cex.axis=cex*0.7,line=0);
  }
  
  if (!is.null(title)){mtext(title,side=3,outer=TRUE,cex=cex,line=-1.5,adj=0.02);}
  mtext(xlab,side=1,outer=TRUE,cex=cex);
  mtext(ylab,side=2,outer=TRUE,cex=cex);
  
  if (showDiagonal) lines(c(0,nrow),c(0,nrow),lty=2,lwd=2,col="blue")
  
  par(mar=c(6,0,6,4))
  ticks<-pretty(c(zmin,zmax),min.n=2);
  zmin<-min(ticks)
  zmax<-max(ticks)
  clrmat<-matrix(data=zmin+(zmax-zmin)*(1:ncolors)/ncolors,nrow=1,ncol=ncolors)
  image(1,1:ncolors,clrmat,col=pal,xlab="",xaxt="n",ylab="",yaxt="n")
  axis(2,at=(1:(ncolors+1))-0.5,labels=FALSE,line=0,lwd.ticks=0);
  axis(3,at=(1:2)-0.5,labels=FALSE,line=0,lwd.ticks=0);
  axis(4,at=ncolors*(ticks-zmin)/(zmax-zmin),labels=ticks,cex.axis=cex*0.9)
  mtext(zlab,side=4,line=2.2,cex=cex);
  
}
