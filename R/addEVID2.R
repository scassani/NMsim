##' Add simulation (sample) records to dosing records (see `NMaddSamples()`)
##' @param ... Passed to `NMaddSamples()`
##' @export
##' @return A data.frame
## Deprecated with NMsim 0.2.1

addEVID2 <- function(...){

    dots <- list(...)
    if(is.null(dots$quiet) || !dots$quiet){
        message("`addEVID2()` has been renamed to `NMaddSamples()`. `addEVID2()` still works to ensure
backward-compatibility.")
    }
    
    NMaddSamples(...)

}
