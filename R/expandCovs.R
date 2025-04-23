##' Create data set where each covariate is univariately varied (see `forestDefineCovs()`)
##' 
##' @param ... Passed to `forestDefineCovs()`
##' @export
##' @return A data.frame
## Deprecated with NMsim 0.2.1

expandCovs <- function(...){

    ## .Deprecated("forestDefineCovs")
    message("`expandCovs()` has been renamed to `(forestDefineCovs)`. `expandCovs()` still works to ensure backward-compatibility.")
    
    forestDefineCovs(...)

}


