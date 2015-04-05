#'
#'@title Read a TCSAM2015 model parameters csv file and return a dataframe.
#'
#'@description Function to read a TCSAM2015 parameters csv file and return a dataframe.
#'
#'@param csvFile - parameters csv file from a TCSAM2015 model run. can be NULL.
#'
#'@return a dataframe (or NULL).
#'
#'@details If csvFile is NULL, the user will be prompted to identify a 
#'TCSAM2015 model parameters csv file to read.
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
readParamsCSV<-function(csvFile=NULL){
    if (!is.character(csvFile)){
        in.prs<-wtsUtilities::selectFile(ext='csv',caption="Select active parameters info csv file");
    } else {
        in.prs<-csvFile;
    }
    obj.prs<-NULL;
    if (!is.null(in.prs)){
        obj.prs<-read.csv(in.prs,stringsAsFactors=FALSE);
    } else {
        cat('No active parameters csv file specified.\n',
            'Returning NULL...\n');
        return(NULL);
    }
    return(obj.prs)
}
