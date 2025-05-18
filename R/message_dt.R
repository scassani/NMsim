##' print a data.table
##'
##' Notice, only data.tables supported

##' @keywords internal
message_dt <- function(dt){
    if(!is.data.table(dt)){
        dt <- as.data.table(dt)
    }
    message(paste(capture.output(print(dt,class=FALSE,print.keys=FALSE)),collapse="\n"))
}
