##' Summarize simulated exposures relative to reference subject (see `forestSummarize()`)
##' @param ... Passed to `forestSummarize()`
##' @export
##' @return A data.frame
## Deprecated with NMsim 0.2.1


### forestDefineCovs
### forestSummarize


summarizeCovs <- function(...){

    message("`summarizeCovs()` has been renamed to `forestSummarize()`. `summarizeCovs()` still works to ensure backward-compatibility.")
    
    forestSummarize(...)
}

