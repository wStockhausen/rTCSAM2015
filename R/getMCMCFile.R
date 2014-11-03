#'
#'@title Get a TCSAM2015 MCMC results file
#'
#'@description Allows the user to select a TCSAM2015 MCMC results file using a file dialog.
#'
#'@return path to selected TCSAM2015 MCMC results file, or NULL (if user canceled selection).
#'
#'@export
#'@importFrom tcltk tk_choose.files
#'@importFrom wtsUtilities addFilter
#'
getMCMCFile<-function(){
    Filters<-wtsUtilities::addFilter("R","R files (*.R)","*.R",Filters=NULL);
    rfile<-tcltk::tk_choose.files(caption="Select TCSAM2015 MCMC Results (.R) file",
                             multi=FALSE,filters=Filters);
    if (length(rfile)==0) return(NULL);
    return(rfile)
}