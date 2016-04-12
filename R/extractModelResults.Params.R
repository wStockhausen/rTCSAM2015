#'
#'@title Function to extract parameter values from different models.
#'
#'@description This function extracts parameters values, together with their limits
#'(if any) from several models.
#'
#'@param objs - list of model results objects (each is a list with elements 'prsObj' and 'stdObj')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param fac - number of std devs to extend uncertainty plots
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - list with two dataframes, one with the parameter info (prsDFR), one with uncertainty info (stdDFR).
#'
#'@details Returned parameter info dataframe has columns case, type, param, init, value, min, max, and scl.
#'
#'@export
#'
extractModelResults.Params<-function(objs,
                                     dp=0.01,
                                     fac=2,
                                     verbose=FALSE){
    pdfr<-NULL;
    sdfr<-NULL;
    cases<-names(objs);
    for (case in cases){
        #loop over model cases
        prs<-objs[[case]]$prsObj;        
        std<-objs[[case]]$stdObj;
        params<-as.character(unique(prs$name));
        for (param in params){
            if (verbose) cat("processing '",param,"'\n",sep='')
            prsp<-prs[prs$name==param,];
            nrp<-nrow(prsp);
            nrs<-0;
            prmw  <- gsub("[[:blank:]]",'',param,fixed=FALSE);#need to remove whitespace
            splt<-strsplit(prmw,"[[:punct:]]");
            prm<-paste(splt[[1]][1],"[",wtsUtilities::formatZeros(splt[[1]][2],width=2),"]",sep='')
            if (nrp>0){
                stdp<-NULL;
                if (!is.null(std)) {
                    idxs<-std[,2]==prmw;
                    stdp<-std[idxs,];
                    nrs<-nrow(stdp);
                    if ((nrs>0)&&verbose) cat("--Found std entries for '",prmw,"'!\n",sep='')
                }
                if (verbose) cat('--nrp =',nrp,' nrs =',nrs,'\n')
                for (r in 1:nrp){
                    ##process only defined parameters 
                    if (!((prsp$phase[r]==-1)&&(prsp$min[r]==-1)&&(prsp$max[r]==-1))){
                        if (verbose) cat('--Processing prs for row',r,'\n')
                        rw<-list();
                        rw$case <-case;
                        rw$type <-'scalar';
                        rw$param<-prm;
                        if (nrp>1) {
                            rw$type <-'vector';
                            rw$param<-paste(prm,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                        }
                        rw$init <-prsp$init[r];
                        rw$value<-prsp$value[r];
                        rw$min  <-prsp$min[r];
                        rw$max  <-prsp$max[r];
                        rw$scl  <-"";
                        if (is.finite(rw$min)){
                            if (abs(rw$value-rw$min)/(rw$max-rw$min)<dp/100) rw$scl <-"near min";
                            if (abs(rw$value-rw$max)/(rw$max-rw$min)<dp/100) rw$scl <-"near max";
                        }#finite limits
                        pdfr<-rbind(pdfr,as.data.frame(rw,stringsAsFactors=FALSE));
                        if (nrs>0){
                            if (verbose) cat("--processing std\n")
                            estp  <-stdp[r,3];#model estimate
                            stdv <-stdp[r,4]; #standard dev
                            if (stdv>0) {
                                if (verbose) cat("----std>0\n")
                                cx <-0.5*(rw$min+rw$max);
                                dx <-rw$max-rw$min;
                                mnx<-estp-fac*stdv;
                                if (is.finite(cx)&&(mnx<cx-1.5*dx)) mnx<-cx-1.5*dx;
                                mxx<-estp+fac*stdv;
                                if (is.finite(cx)&&(mxx>cx+1.5*dx)) mxx<-cx+1.5*dx;
                                x<-seq(from=mnx,to=mxx,length.out=51);
                                y<-0.9*exp(-0.5*((x-estp)/stdv)^2);
                                vri<-list(case=rw$case,type=rw$type,par=prm,param=rw$param,x=x,y=y);
                                sdfr<-rbind(sdfr,as.data.frame(vri,stringsAsFactors=FALSE));
                            }#stdv>0
                        }#nrs>0
                    }#parameter defined
                }#nrp
            }#nrp>0
        }#params
    }#case
    return(invisible(list(prsDFR=pdfr,stdDFR=sdfr)));
}
