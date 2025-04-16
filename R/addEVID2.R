##' @rdname NMsample

## Deprecated with NMsim 0.2.1

addEVID2 <- function(...){

    dots <- list(...)
    if(!is.null(dots$quiet) && !dots$quiet){
        message("`addEVID2()` has been renamed to `NMsample()`. `addEVID2()` still works to ensure
backward-compatibility.")
    }
    
    NMsample(...)

}
