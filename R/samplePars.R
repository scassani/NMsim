##' Sample model parameters using `mvrnorm` or the `simpar` package
##' @param file.mod Path to model control stream. Will be used for
##'     both `NMreadExt()` and `NMreadCov()`, and extension will
##'     automatically be replaced by `.ext` and `.cov`.
##' @param nsims Number of sets of parameter values to
##'     generate. Passed to `simpar`.
##' @param method The sampling method. Options are "mvrnorm" and
##'     "simpar". Each have pros and cons. Notice that both methods
##'     are fully automated as long as ".ext" and ".cov" files are
##'     available from model estimation.
##' @param format The returned data set format "ext" (default) or
##'     "wide". "ext" is a long-format, similar to what
##'     `NMdata::NMreadExt()` returns.
##' @param seed.R seed value passed to set.seed().
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##'
##' @details samplePars() uses internal methods to sample using
##'     mvrnorm or simpar. Also be aware of NMsim_NWPRI which is based
##'     on the Nonmem-internal NWPRI subroutine. NMsim_NWPRI is much
##'     faster to execute.  Simulation with paramater uncertainty on
##'     variance components (`OMEGA` and `SIGMA`) is only reliable
##'     starting from Nonmem 7.6.0.
##'
##' mvrorm: The multivariate normal distribution does not ensure
##' non-negative variances. Negative variances are not allowed and can
##' not be simulated. To avoid this, `method=mvrnorm` truncates negative variance diagonal
##' elements at zero.
##'
##' simpar: simpar must be installed. 
##'
##' Please refer to publications and vignettes for more information on
##' sampling methods.
##' @author Sanaya Shroff, Philip Delff
##' @import NMdata
##' @import data.table
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
