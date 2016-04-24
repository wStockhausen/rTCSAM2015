#'
#'@title Calculate log-prior distribution based on input pdfType list
#'
#'@description Function to calculate log-prior distribution for plotting based on an input pdfType list.
#'
#'@param pdfType - the pdfType list
#'@param v - values at which to calculate the prior values
#'@param x - vector of x coordinates used to plot the resulting distribution as a color image
#'@param lci - lower confidence interval for plotting the prior (if v is NULL)
#'@param uci - upper confidence interval for plotting the prior (if v is NULL)
#'
#'@return see details
#'
#'@details If x is NULL, the function returns values of the log-prior distribution function 
#'calculated using v (or coordinates based on the confidence interval). The names of the vector
#'provide the values of v at which the log prior was calculated. If x is given, the function returns
#'a list object of class 'pdfRects', with list elements lp, xl, xr, vl, and vr.
#'
#'@import stats
#'
#'@export
#'
calcLogPrior<-function(pdfType,v=NULL,x=NULL,lci=0.05,uci=0.95){
    if (pdfType$type=='normal'){
        if (is.null(v)) {
            lims<-qnorm(c(lci,uci),mean=pdfType$params$mean,sd=pdfType$params$stdv,lower.tail=TRUE);
            v<-seq(from=lims[1],to=lims[2],length.out=100);
        }
        lp<-dnorm(v,mean=pdfType$params$mean,sd=pdfType$params$stdv,log=TRUE);
        if (is.null(x)){
            #return log prior as a vector
            names(lp)<-as.character(v);
            return(lp);
        } else {
            #return log prior as list of 'rects' class
            nx<-length(x)
            xl<-c(x[1]-0.5*(x[2]-x[1]),0.5*(x[2:nx]+x[1:(nx-1)]));
            xr<-c(xl[2:(nx+1)],x[nx]+0.5*(x[nx]-x[nx-1]));
            nv<-length(v);
            vl<-c(v[1]-0.5*(v[2]-v[1]),0.5*(v[2:nv]+v[1:(nv-1)]));
            vu<-c(vl[2:(nv+1)],v[nv]+0.5*(v[nv]-v[nv-1]));
            rects<-list(lp=lp,xl=xl,xr=xr,vl=vl,vu=vu);
            class(rects)<-'pdfRects';
            return(rects);
        }
    } else if (pdfType$type=='expnormal'){
        if (is.null(v)) {
            lims<-qnorm(c(lci,uci),mean=pdfType$params$mean,sd=pdfType$params$stdv,lower.tail=TRUE);
            v<-seq(from=lims[1],to=lims[2],length.out=100);
        }
        lp<-dnorm(exp(v),mean=pdfType$params$expmean,sd=pdfType$params$stdv,log=TRUE);
        if (is.null(x)){
            #return log prior as a vector
            names(lp)<-as.character(v);
            return(lp);
        } else {
            #return log prior as list of 'rects' class
            nx<-length(x)
            xl<-c(x[1]-0.5*(x[2]-x[1]),0.5*(x[2:nx]+x[1:(nx-1)]));
            xr<-c(xl[2:(nx+1)],x[nx]+0.5*(x[nx]-x[nx-1]));
            nv<-length(v);
            vl<-c(v[1]-0.5*(v[2]-v[1]),0.5*(v[2:nv]+v[1:(nv-1)]));
            vu<-c(vl[2:(nv+1)],v[nv]+0.5*(v[nv]-v[nv-1]));
            rects<-list(lp=lp,xl=xl,xr=xr,vl=vl,vu=vu);
            class(rects)<-'pdfRects';
            return(rects);
        }
    } else if (pdfType$type %in% c('1stdiff_normal','ar1_normal')){
        if (is.null(v)) {
            lims<-qnorm(c(lci,uci),mean=pdfType$params$mean,sd=pdfType$params$stdv,lower.tail=TRUE);
            v<-seq(from=lims[1],to=lims[2],length.out=100);
        }
        lp<-dnorm(v,mean=pdfType$params$mean,sd=pdfType$params$stdv,log=TRUE);
        #return log prior as list of 'rects' class
        nx<-length(x)
        xl<-c(x[1]-0.5*(x[2]-x[1]),0.5*(x[2:nx]+x[1:(nx-1)]));
        xr<-c(xl[2:(nx+1)],x[nx]+0.5*(x[nx]-x[nx-1]));
        nv<-length(v);
        vl<-c(v[1]-0.5*(v[2]-v[1]),0.5*(v[2:nv]+v[1:(nv-1)]));
        vu<-c(vl[2:(nv+1)],v[nv]+0.5*(v[nv]-v[nv-1]));
        rects<-list(lp=lp,xl=xl,xr=xr,vl=vl,vu=vu);
        class(rects)<-'pdfRects';
        return(rects);
    } else {
        cat('pdfType "',pdfType$type,'" not recognized in calcLogPrior().\n');
        cat("Returning NULL.\n");
    }
}