##' Check a simulation control streams for things that can cause
##' trouble in NMsim
##' @param file.mod A control stream to check
##' @param lines The control stream as text lines. Only use of of
##'     `file.mod` and `lines`.
##' @return NULL
##' @keywords internal Don't export

NMsimCheckMod <- function(file.mod,lines){
    
    pk <- NULL
    pred <- NULL
    
    if(missing(lines)) lines <- NULL
    if(missing(file.mod)) file.mod <- NULL
### this is assuming there is only one file, or that lines contains only one control stream.
    lines <- NMdata:::getLines(file=file.mod,lines=lines)
    
    sections.mod <- NMreadSection(lines=lines)
    names(sections.mod) <- tolower(names(sections.mod))
    secnames <- copy(names(sections.mod))

#### PRED_IGNORE_DATA_TEST is OK starting from 0.2.0 with NMROW approach
    ## lines.pred.or.pk <- c(sections.mod[["pk"]],sections.mod[["pred"]])
    ## if(!is.null(lines.pred.or.pk)){
    ##     if(any(grepl("PRED_IGNORE_DATA_TEST",lines.pred.or.pk))){
    ##         warning("PRED_IGNORE_DATA_TEST found in $PK or $PRED. If the ignore condition matches data records, they will be dropped. Consider if you can avoid this. If intended, you must still make sure that simulation input and output data is merged by a row identifier. See  ")
    ##         }
    ## }

    
    if("error"%in%secnames){
        ##if(any(grepl("OBSERVATIONS +ONLY",sections.mod[["error"]]) |
        ##grepl("OBS +ONLY",sections.mod[["error"]]))
        if(any(grepl("(OBS|OBSERVATIONS) +ONLY",sections.mod[["error"]]))){
            warning("OBSERVATIONS ONLY used in $ERROR. Predictions may only be evaluated for EVID=0 records.")
        }
    }
    return(invisible(NULL))
}
