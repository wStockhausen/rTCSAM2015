#'
#'@title Read a TCSAM 2015 model report (.rep) file.
#'
#'@description Function to read a TCSAM 2015 report file (.rep).
#'
#'@param resfile - results file from a TCSAM2014 model run to source. can be NULL.
#'
#'@return results model object as list. The returned list will be of class 'tcsam2015'.
#'
#'@details If resfile is NULL, the user will be prompted to identify a 
#'TCSAM 2015 model report file from which to source the results list object.
#'
#'@export
#'
getModelResults<-function(resfile=NULL){
    if(is.null(resfile)){
        resfile = getResultsFile();
        if (is.null(resfile)) {
            cat("User canceled file selection!! Returning NULL as model results.\n")
            return(NULL);#user canceled file selection
        }
        strs<-strsplit(resfile,'.',fixed=TRUE);
        n<-length(strs[[1]]);
        if (tolower(strs[[1]][n])!="rep"){
        }
    }
    cat("reading model results from file:\n",resfile,"\n")
    source(resfile,local=TRUE);
    if(!any(names(res)=='mc')){
            cat("The file '",resfile,"'",
                "     does not appear to be a TCSAM2015 model results file.",
                "TCSAM2015 results files are R lists, with 'mc' as the first element.",
                "Returning NULL.",sep="\n");
            return(NULL);
    }
    class(res)<-'tcsam2015';#set class attribute to 'tcsam2015' for identification
    return(invisible(res));
}