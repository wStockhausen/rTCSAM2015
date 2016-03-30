#'
#'@title Get a TCSAM2015 std object by reading a .std file
#'
#'@description Function to get a TCSAM2015 std object by reading a .std file
#'
#'@param csvFile - parameters csv file from a TCSAM2015 model run. can be NULL.
#'
#'@return std model object (a list). The returned object will be a list of class 'tcsam2015.std'.
#'
#'@details If \code{csvFile} is NULL, the user will be prompted to identify a 
#'TCSAM2015 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam2015.std'.
#'
#'@export
#'
getStd<-function(stdFile){
    stdObj<-readStdFile(stdFile);
    return(stdObj);
}
