#'
#'@title Read a TCSAM2015 MCMC (.R) file
#'
#'@description Function to read a TCSAM2015 MCMC (.R) file.
#'
#'@param mcmcfile - results file from a TCSAM2015 MCMC run to source. can be NULL.
#'
#'@return MCMC object as list. The returned list will be of class 'tcsam2015.mcmc'.
#'
#'@details If resfile is NULL, the user will be prompted to identify a 
#'TCSAM2015 MCMC file from which to source the results list object.
#'
#'@export
#'
getMCMCResults<-function(mcmcfile=NULL){
    mcmc<-NULL;
    if(is.null(mcmcfile)){
        mcmcfile = getMCMCFile();
        if (is.null(mcmcfile)) {
            cat("User canceled file selection!! Returning NULL as MCMC results.\n")
            return(NULL);#user canceled file selection
        }
        strs<-strsplit(mcmcfile,'.',fixed=TRUE);
        n<-length(strs[[1]]);
        if (tolower(strs[[1]][n])!="R"){
        }
    }
    cat("reading MCMC results from file:\n",mcmcfile,"\n")
    source(mcmcfile,local=TRUE);
    if(is.null(mcmc)){
            cat("The file '",mcmcfile,"'",
                "     does not appear to be a TCSAM2015 MCMC results file.",
                "An TCSAM2015 MCMC files is an R file with 'mcmc' as the name of the single list object.",
                "Returning NULL.",sep="\n");
            return(NULL);
    }
    n<-length(mcmc);
    mcmc<-mcmc[1:(n-1)];
    class(mcmc)<-'tcsam2015.mcmc';#set class attribute to 'tcsam2015mcmc' for identification
    return(invisible(mcmc));
}