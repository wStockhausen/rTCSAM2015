#'
#'@title Read a TCSAM2015 model report (.rep) file.
#'
#'@description Function to read a TCSAM2015 report file (.rep).
#'
#'@param repfile - report file from a TCSAM2015 model run to source. can be NULL.
#'
#'@return report model object (a list). The returned object will be a list of class 'tcsam2015.rep'.
#'
#'@details If repfile is NULL, the user will be prompted to identify a 
#'TCSAM2015 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam2015.rep'.
#'
#'@export
#'
readReportFile<-function(repfile=NULL){
    res<-NULL;
    if(is.null(repfile)){
        repfile<-selectReportFile();
        if (is.null(repfile)) {
            cat("User canceled file selection!! Returning NULL as model results.\n")
            return(NULL);#user canceled file selection
        }
        strs<-strsplit(repfile,'.',fixed=TRUE);
        n<-length(strs[[1]]);
        if (tolower(strs[[1]][n])!="rep"){
            cat("The file '",repfile,"'\n",
                "\tdoes not appear to be a TCSAM2015 model report file.\n",
                "\tTCSAM2015 report files have extension '.rep'.\n",
                "\tReturning NULL.\n",sep="");
            return(NULL);
        }
    }
    if (file.exists(repfile)){
        cat("Reading model report from file:\n",repfile,"\n")
        source(repfile,local=TRUE);
        if(!any(names(res)=='mc')){
                cat("The file '",repfile,"'\n",
                    "\tdoes not appear to be a TCSAM2015 model report file.\n",
                    "\tTCSAM2015 results files are R lists, with 'mc' as the first element.\n",
                    "\tReturning NULL.\n",sep="");
                return(NULL);
        }
        class(res)<-c('tcsam2015.rep',class(res));#set class attribute to 'tcsam2015.rep' for identification
    } else {
        cat('\tFile "',repfile,'" does not exist.\n\tReturning NULL\n',sep='');
    }
    return(invisible(res));
}
