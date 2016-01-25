#'
#'@title Function to extract parameter values from different models.
#'
#'@description This function extracts parameters values, together with their limits
#'(if any) from several models.
#'
#'@param objs - list of model results objects (each is a list with elements 'prsObj' and 'stdObj')
#'@param dp - percent difference between parameter value and upper/lower limits used to flag outliers
#'@param verbose - flag (T/F) to print diagnostic info
#'
#'@return - dataframe with the parameter information
#'
#'@export
#'
extractModelResults.Params<-function(objs,
                                     dp=0.01,
                                     verbose=FALSE){
    dfr<-NULL;
    cases<-names(objs);
    for (case in cases){
        #loop over model cases
        prs<-objs[[case]]$prsObj;        
        params<-as.character(unique(prs$name));
        for (param in params){
            prsp<-prs[prs$name==param,];
            nr<-nrow(prsp);
            if (nr>0){
                for (r in 1:nr){
                    rw<-list();
                    rw$case <-case;
                    rw$type <-'scalar';
                    prmw  <- gsub("[[:blank:]]",'',param,fixed=FALSE);#need to remove whitespace
                    splt<-strsplit(prmw,"[[:punct:]]");
                    prm<-paste(splt[[1]][1],"[",wtsUtilities::formatZeros(splt[[1]][2],width=2),"]",sep='')
                    rw$param<-prm;
                    if (nr>1) {
                        rw$type <-'vector';
                        rw$param<-paste(prm,"[",wtsUtilities::formatZeros(r,width=2),"]",sep='');
                    }
                    rw$value<-prsp$value[r];
                    rw$min  <-prsp$min[r];
                    rw$max  <-prsp$max[r];
                    rw$scl  <- 0;
                    if (is.finite(rw$min)){
                        if (abs(rw$value-rw$min)/(rw$max-rw$min)<dp/100) rw$scl <- -1;
                        if (abs(rw$value-rw$max)/(rw$max-rw$min)<dp/100) rw$scl <-  1;
                    }#finite limits
                    dfr<-rbind(dfr,as.data.frame(rw,stringsAsFactors=FALSE));
                }#r
            }#nr>0
        }#params
    }#case
    return(invisible(dfr));
}
