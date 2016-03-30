#'
#'@title Get a TCSAM2015 prs object by reading a parameters csv file
#'
#'@description Function to get a TCSAM2015 prs object by reading a parameters csv file.
#'
#'@param csvFile - parameters csv file from a TCSAM2015 model run. can be NULL.
#'
#'@return prs model object (a list). The returned object will be a list of class 'tcsam2015.prs'.
#'
#'@details If \code{csvFile} is NULL, the user will be prompted to identify a 
#'TCSAM2015 model report file from which to source the results object.
#'The returned object will be a list of class 'tcsam2015.prs'.
#'
#'@export
#'
getPrs<-function(csvFile){
    prsObj<-readParamsCSV(csvFile);
    return(prsObj);
}
