#'
#'@title Get a TCSAM2015 model results file
#'
#'@description Allows the user to select a TCSAM2015 model results file using a file dialog.
#'
#'@return path to selected TCSAM2015 model results file, or NULL (if user canceled selection).
#'
#'@export
#'@importFrom tcltk tk_choose.files
#'@importFrom wtsUtilities addFilter
#'
getResultsFile<-function(){
    Filters<-wtsUtilities::addFilter("rep","report files (*.rep)","*.rep",Filters=NULL);
    Filters<-wtsUtilities::addFilter("R","R files (*.R)","*.R",Filters=Filters);
    Filters<-wtsUtilities::addFilter("r*","report files (*.r*)","*.r*",Filters=Filters);
    rfile<-tcltk::tk_choose.files(caption="Select TCSAM2015 R Model Results (.rep or .R) file",
                             multi=FALSE,filters=Filters);
    if (length(rfile)==0) return(NULL);
    return(rfile)
}