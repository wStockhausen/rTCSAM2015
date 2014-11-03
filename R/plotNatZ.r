#'
#'@title Plot numbers-at-size as circles
#'
#'@description Plots numbers at size (5-d array with dim.s 
#'sex,maturity state, shell condition, year, size) as circles
#'
#'@param nAtZ - 5-d array with dim.s sex,maturity state, shell condition, year, size
#'
#'@export
#'
plotNatZ<-function(nAtZ){
    #extract dimensions of nAtZ
    dim.names<-dimnames(nAtZ);
    sxs<-dim.names[[1]];
    mss<-dim.names[[2]]
    scs<-dim.names[[3]]
    yrs<-as.numeric(dim.names[[4]]);
    zs<-as.numeric(dim.names[[5]]);
    
    for (sex in c("FEMALE","MALE")){
        n.imm<-as.matrix(nAtZ[sex,"IMMATURE","NEW_SHELL",,])
        n.mat<-as.matrix(nAtZ[sex,  "MATURE","NEW_SHELL",,])+
                as.matrix(nAtZ[sex, "MATURE","OLD_SHELL",,]);
        
        zscl<-max(n.imm,n.mat)
        plotCompsAsCircles(z=t(n.imm),x=yrs,y=zs,overplot=FALSE,bg='blue',
                           xlab='',ylab='mm CW',
                           scale=zscl,maxRadius=0.8)
        plotCompsAsCircles(z=t(n.mat),x=yrs,y=zs,overplot=TRUE,
                           bg='green',scale=zscl,maxRadius=0.8)
        mtext(paste(tolower(sex),'s',sep=''),side=3,adj=0.05)
    }
}

#plotNatZ(nAtZ)
