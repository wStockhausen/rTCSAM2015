#'
#'@title Function to extract parameter estimates and standard deviations from different TCSAM2015 models
#'
#'@description This function extracts parameters values and standard deviations, together with their limits
#'(if any) from several TCSAM2015 models.
#'
#'@param objs - list of TCSAM2015 model results objects (each is a list with elements 'prsObj' and 'stdObj')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - dataframe with the parameter estimates and uncertainty info.
#'
#'@details Returned parameter info dataframe has columns case, type, param, min, max, init, value, stdvv and scl.
#'
#'@export
#'
extractModelResults.ParamsPlusStdDevs<-function(objs,
                                                dp=0.01,
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
                        rw$min  <-prsp$min[r];
                        rw$max  <-prsp$max[r];
                        rw$init <-prsp$init[r];
                        rw$value<-prsp$value[r];
                        rw$stdv <-0.0;
                        if (nrs>0){
                            if (verbose) cat("--processing std\n")
                            rw$stdv <-stdp[r,4]; #standard dev
                        }#nrs>0
                        rw$scl  <-"";
                        if (is.finite(rw$min)){
                            if (abs(rw$value-rw$min)/(rw$max-rw$min)<dp/100) rw$scl <-"near min";
                            if (abs(rw$value-rw$max)/(rw$max-rw$min)<dp/100) rw$scl <-"near max";
                        }#finite limits
                        pdfr<-rbind(pdfr,as.data.frame(rw,stringsAsFactors=FALSE));
                    }#parameter defined
                }#nrp
            }#nrp>0
        }#params
    }#case
    return(pdfr);
}
