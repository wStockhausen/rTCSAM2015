#'
#'@title Function to extract parameter uncertainty (std) info from different models.
#'
#'@description This function extracts the posterior distributions of parameters and
#'sdreport variables as implied by their estimated standard errors from several models.
#'
#'@param objs - model results objects (each is a list with elements 'stdObj')
#'@param fac - number of std devs to extend uncertainty plots
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - dataframe with uncertainty info
#'
#'@details Uses function \code{wtsUtilities::formatZeros(...)}.
#'
#'@export
#'
extractModelResults.StdDevs<-function(objs,
                                      fac=2,
                                      verbose=FALSE){
    vfr<-NULL;
    cases<-names(objs);
    for (case in cases){   
        #loop over model cases
        std<-objs[[case]]$stdObj;
        params<-as.character(unique(std[,2]));
        for (param in params){
            #loop over param names
            idx<-std[,2]==param;
            stdp<-std[idx,];
            nr<-nrow(stdp);
            if (nr>0){
                type<-'scalar';
                prmw  <- gsub("[[:blank:]]",'',param,fixed=FALSE);#need to remove whitespace
                splt<-strsplit(prmw,"[[:punct:]]");
                prm<-paste(splt[[1]][1],"[",wtsUtilities::formatZeros(splt[[1]][2],width=2),"]",sep='')
                paramp<-prm;
                for (r in 1:nr){
                    par   <-prm;      #param name
                    estp  <-stdp[r,3];#model estimate
                    stdv <-stdp[r,4]; #standard dev
                    if (stdv>0) {
                        x<-seq(from=estp-fac*stdv,to=estp+fac*stdv,length.out=51);
                        y<-0.9*exp(-0.5*((x-estp)/stdv)^2);
                        if (nr>1) {
                            type<-'vector';
                            paramp<-paste(prm,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                        }
                        vri<-list(case=case,type=type,par=par,param=paramp,x=x,y=y);
                        vfr<-rbind(vfr,as.data.frame(vri,stringsAsFactors=FALSE));
                    }#stdv>0
                }#r
            }#nr>0
        }#params
    }#case
    return(invisible(vfr));
}

