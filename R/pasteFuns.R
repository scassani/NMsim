##' Paste string to start of vector only
##'
##' paste(str,x) will prepend str to all values of x. use pasteBegin
##' to only paste it to the first value of x.
##' @param x A vector of strings
##' @param add A string to add
##' @param ... Aditional arguments to `paste()`.
##' @name pasteFuns
##' @keywords internal

##' @rdname pasteFuns
pasteBegin <- function(x,add,...){
    res <- paste(add,x[1],...)
    if(length(x)>1){
        res <- c(res,x[2:(length(x))])
    }
    res
}

##' @rdname pasteFuns
pasteEnd <- function(x,add,...){
    c(x[0:(length(x)-1)],
      paste(x[length(x)],add,...)
      )
}
