#'
#'@title Read a TCSAM2015 model std file and return a dataframe.
#'
#'@description Function to read a TCSAM2015 std file and return a dataframe.
#'
#'@param stdfile - std file from a TCSAM2015 model run. can be NULL.
#'
#'@return a dataframe (or NULL).
#'
#'@details If stdFile is NULL, the user will be prompted to identify a 
#'TCSAM2015 model std file to read.
#'
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
readStdFile<-function(stdFile=NULL){
    if (!is.character(stdFile)){
        in.std<-wtsUtilities::selectFile(ext='std',caption="Select TCSAM2015 std file");
    } else {
        in.std<-stdFile;
    }
    obj.std<-NULL;
    if (!is.null(in.std)) {
        obj.std = read.table(in.std,as.is=T,header=F,skip=1);
        colnames(obj.std)<-c("row id","name","est","std.dev")
    } else {
        cat('No std file specified.\n',
            'Returning NULL...\n');
        return(NULL);
    }
    return(obj.std)
}
