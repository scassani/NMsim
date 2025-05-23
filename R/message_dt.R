##' print a data.table
##'
##' @param dt a data.table or something to be converted to a data.table.


##' @keywords internal
message_dt <- function(dt){
    if(!is.data.table(dt)){
        dt <- as.data.table(dt)
    }
    message(paste(capture.output(print(dt,class=FALSE,print.keys=FALSE)),collapse="\n"))
}
