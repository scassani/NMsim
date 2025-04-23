##' Replace initial values in Nonmem control stream
##' @param inits A data.frame with new initial estimates, same style
##'     as returned by NMdata::NMreadExt. Column` par.type` can contain
##'     elements THETA, OMEGA, SIGMA.
##' @param ... Passed to NMdata::NMwriteSection. This is important for
##'     NMreplaceInits to run at all.
##' @return The modified control stream
##' @keywords internal

NMreplaceInits <- function(inits,fix=FALSE,...){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    FIX <- NULL
    i <- NULL
    j <- NULL
    par.type <- NULL
    str.fix <- NULL
    value <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    
    ## create THETA section
    inits <- copy(inits)
    setDT(inits)
    if(!"FIX"%in%colnames(inits)) {
        inits[,FIX:=0]
        ##inits$FIX <- 0
    }
    thetas <- inits[par.type=="THETA"]
    setorder(thetas,i)
    
    ## thetas[,str.fix:=fifelse(as.logical(FIX)," FIX","")]
    thetas[,str.fix:=""]
    thetas[as.logical(FIX)==TRUE,str.fix:=" FIX"]
    lines.theta <- c("$THETA",
                     thetas[,paste0(value,str.fix)]
                     )

    
    ## create OMEGA section
    omegas <- inits[par.type=="OMEGA"]

    lines.omega <- NMcreateMatLines(omegas,type="OMEGA",as.one.block=FALSE,fix=fix)
    sigmas <- inits[par.type=="SIGMA"]    
    lines.sigma <- NMcreateMatLines(sigmas,type="SIGMA",as.one.block=FALSE,fix=fix)


    list.sections <- list(THETA=lines.theta
                         ,OMEGA=lines.omega
                         ,SIGMA=lines.sigma)


    dots <- list(...)
    if(!"quiet"%in%names(dots)) {
        dots$quiet <- TRUE
    }
    if(!"backup"%in%names(dots)) {
        dots$backup <- TRUE
    }
    dots$list.sections <- list.sections
    ## res <- NMwriteSection(list.sections=list.sections
    ##                      ,quiet=TRUE
    ##                      ,...
    ##                       )
    res <- do.call(NMwriteSection,dots)
    
    invisible(res)
}
