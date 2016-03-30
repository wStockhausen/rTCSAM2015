#'
#'@title Get a TCSAM2015 report object (by reading a TCSAM2015 model report [.rep] file)
#'
#'@description Function to get a TCSAM2015 report object
#'
#'@param repFile - report file from a TCSAM2015 model run to source. can be NULL.
#'
#'@return TCSAM2015 report object (a list). The returned object will be a list of class 'tcsam2015.rep'.
#'
#'@details If \code{repFile} is NULL, the user will be prompted to identify a 
#'TCSAM2015 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam2015.rep'.
#'
#'@export
#'
getRep<-function(repFile){
    repObj<-readReportFile(repFile);
    return(repObj);
}
