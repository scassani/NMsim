##' Sample model parameters using `mvrnorm` or the `simpar` package
##' @param file.mod Path to model control stream. Will be used for
##'     both `NMreadExt()` and `NMreadCov()`, and extension will
##'     automatically be replaced by `.ext` and `.cov`.
##' @param nsims Number of sets of parameter values to
##'     generate. Passed to `simpar`.
##' @param method The sampling method. Options are "mvrnorm" and
##'     "simpar". Each have pros and cons. Notice that both methods
##'     are fully automated as long as ".ext" and ".cov" files are
##'     available.
##' @param format The returned data set format "ext" (default) or
##'     "wide". "ext" is a long-format, similar to what
##'     `NMdata::NMreadExt()` returns.
##' @param seed.R seed value passed to set.seed().
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @details This function was originally create by Sanaya Shroff and
##'     Philip Delff for sampling using `simpar`. It has since been
##'     generalized to support sampling with `mvrnorm()` too.
##' @import NMdata
##' @return A table with sampled model parameters
##' @export


samplePars <- function(file.mod,nsims,method,seed.R,format="ext",as.fun){
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    if(!missing(seed.R) && !is.null(seed.R)) {
        set.seed(seed=seed.R)
    }

    args <- list(file.mod,nsims,format=format,as.fun="data.table")
    
    pars <- switch(method,
                   mvrnorm=do.call(samplePars_mvrnorm,args),
                   simpar=do.call(samplePars_simpar,args)
                   )

    if(format=="ext"){
        cols.order <- c("model","model.sim","parameter","value","par.type","i","j","value.est","FIX","iblock","blocksize")
        cols.order <- intersect(cols.order,colnames(pars))
        setcolorder(pars,cols.order)
    }
    
    as.fun(pars)
    
}
