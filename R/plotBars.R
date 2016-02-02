#'
#'@title Create or add a bar plot.
#'
#'@description Function to create or add to a bar plot.
#'
#' @param y - matrix of values for bars (group elements as rows, series as columns)
#' @param x - x coordinate for bars (groups) (optional)
#' @param w - bar (group) width (or widths, if a vector) (optional)
#' @param xloc - type of x location specified by x."L" - left cutpoint, "C" - center point, "R" - right cutpoint
#' @param df          - dataframe (optional way to specify x, y)
#' @param xCol        - column name in df to use as x
#' @param yCol        - vector of column names to use as y
#' @param overplot    - TRUE to plot bars on last graph
#' @param stack       - TRUE to stack bars
#' @param bar width   - scale (in percent) to use for bar widths
#' @param groupwidths - scale (in percent) to use for group widths
#' @param barclr      - vector of bar colors by group
#' @param ymin        - min y to plot
#' @param plotLegend  - TRUE/FALSE flag to plot legend
#' @param leg.loc     - location for legend (e.g., "topright")
#' @param leg.text    - text labels for legend
#'
#'@details Uses functions
#'\itemize{
#'  \item wtsUtilities::mod(...)
#'}
#'
#'@export
plotBars<-function(y=NULL,
                   x=NULL,
                   w=NULL,
                   df=NULL,
                   xCol=NULL,
                   yCol=NULL,
                   xloc='L',
                   xlabs=NA,
                   overplot=FALSE,
                   stack=FALSE,
                   barwidth=100,
                   groupwidth=100,
                   barclr=NULL,
                   ymin=0,
                   plotLegend=TRUE,
                   leg.loc="topright",
                   leg.text='',
                   ...) {

    if (!is.null(df)&(is.data.frame(df))) {
        if (!is.null(xCol)) x<-df[,xCol];
        y<-df[,yCol];
    }

    #make sure y is a matrix
    y<-as.matrix(y);
    ny<-dim(y)[1];
    ng<-dim(y)[2];
    
    if (is.null(x)) x<-1:ny;

    nx<-length(x);
    if (nx!=ny){
        cat("Error in plotBars(...):\n",
            "Number of rows in y and length of x must agree!\n",
            "nrows(y) = ",ny,"\n",
            "length(x) = ",nx,"\n",
            "Aborting!!",sep='');
        return();
    }

    if (is.null(w)){
        w<-0*x;
        if (toupper(xloc)=='L') {
            w[1:(nx-1)]<-x[2:nx]-x[1:(nx-1)];
            w[nx]<-x[nx]-x[nx-1];
        } else if (toupper(xloc)=='C') {
            w[1:(nx-1)]<-x[2:nx]-x[1:(nx-1)];
            w[nx]<-x[nx]-x[nx-1];
        } else if (toupper(xloc)=='R') {
            w[1]<-x[2]-x[1];
            w[2:nx]<-x[2:nx]-x[1:(nx-1)];
        }
    } else if (length(w)!=nx){
        cat("Error in plotBars(...):\n",
            "Lengths of w and x must agree!\n",
            "length(w) = ",length(w),"\n",
            "length(x) = ",nx,"\n",
            "Aborting!!",sep='');
        return();
    }

    #determine
    if (xloc=='L') {
        xleft<-x;
        xright<-x+w;
    } else if (xloc=='C') {
        xleft<-x-0.5*w;
        xright<-x+0.5*w;
    } else if (xloc=='R') {
        xleft<-x-w;
        xright<-x;
    }

    xlims<-range(c(xleft,xright));
    ylims<-c(ymin,max(y));
    if (stack) {ylims<-c(ymin,ymin+max(rowSums(y)));}

    if (!overplot) {
        plot(xlims,ylims,type='n',xaxt='n',...);
    }

    if (is.null(barclr)) {
        barclr=c("white","grey","red","blue","green","yellow","cyan");
    }

    for (i in 1:nx) {
        ypbottom<-0;
        for (g in 1:ng) {
            if (!stack) {
                dx<-(groupwidth/100)*(w[i]/ng); #width for group
                if (toupper(xloc)=='L'){
                    xpleft<-xleft[i]+(g-1)*dx;      #left side x coord
                } else if (toupper(xloc)=='C'){
                    xpleft<-xleft[i]+(g-1)*dx+(1-(groupwidth/100))*(w[i]/ng)/2; #left side x coord
                } else if (toupper(xloc)=='L'){
                    xpleft<-xleft[i]+(g-1)*+dx;     #left side x coord
                }
                xpright<-xpleft+(barwidth/100)*dx;#right side x coord
                yptop<-y[i,g];
            } else {
                dx<-(groupwidth/100)*(w[i]/ng); #width for group
                xpleft<-xleft[i]+dx;            #left side x coord
                xpright<-xpleft+dx;             #right side x coord
                if (g==1) {
                    yptop<-y[i,g];
                } else {
                    ypbottom<-ypbottom+y[i,g-1];
                    yptop<-yptop+y[i,g];
                }
            }
            #cat(i,g,xpleft,xpright,ypbottom,yptop,"\n",sep=" ")
            rect(xpleft,ypbottom,xpright,yptop,
                 col=barclr[1+wtsUtilities::mod(g-1,length(barclr))])
        }
    }
    
    if (is.list(xlabs)){
        cat('xlabs is a list\n')
        nxl<-length(xlabs);
        axis(1,at=x,labels=xlabs[[1]],cex=0.8);
        if (nxl>1){
            for (i in 2:nxl){
                nxp<-length(xlabs[[i]]);
                dxp<-(xlims[2]-xlims[1])/nxp;
                xp<-xlims[1]+(0:(nxp-1))*dxp+dxp/2;
                axis(1,line=0.6*i,at=xp,labels=xlabs[[i]],tick=FALSE,cex=0.8);
            }
        }
    } else if (is.vector(xlabs)){
        axis(1,at=x,labels=xlabs)
    }
    
    if (plotLegend){
        legend(leg.loc,leg.text,fill=barclr[1:ny],col=barclr[1:ny],cex=0.7)
    }
}
