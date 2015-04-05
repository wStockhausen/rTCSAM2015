#'
#'@title Append a string to another, using a separator, without appending a duplicate.
#'
#'@description Function to append one string ('add') to another ('orig'), using a separator, without appending a duplicate.
#'
#'@param orig - original string to append to
#'@param app - string to append
#'@param sep - string used to separate the original and appended strings. (default is newline)
#'
#'@details The following examples apply (taking sep='/'):\cr
#'1: orig = '',     app = 'app',  result = 'app'\cr
#'2: orig = 'orig', app = 'app',  result = 'orig/app'\cr
#'3: orig = 'orig', app = 'orig', result = 'orig'
#'
#'@export
#'
appendString<-function(orig,app,sep='\n'){
    ifelse((orig=='')||(orig==app),orig<-app,orig<-paste(orig,app,sep=sep))
    return(orig)
}