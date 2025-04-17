##' Expand a set of covariate values into a data.set with reference
##' value
##' @param covlist A covariate specififed in a list. See
##'     ?expandCovLists.
##' @param data See ?expandCovLists.
##' @param col.id The subject ID column name. Necessary because
##'     quantiles sould be quantiles of distribution of covariate on
##'     subjects, not on observations (each subject contributes once).
##' @param sigdigs Used for rounding of covariate values if using
##'     quantiles or if using a function to find reference.
##' @keywords internal
##' @examples
##'     NMsim:::completeCov(covlist=list(covvar="WEIGHTB",values=c(30,60,90),ref=50),sigdigs=3)
##' @importFrom stats quantile

completeCov <- function(covlist,data,col.id="ID",sigdigs=2){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ## quantile <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    
    if(!is.null(covlist$values) && !is.null(covlist$quantiles) ) stop("Please provide values _or_ quantiles, not both.")
    if(is.null(covlist$values) && is.null(covlist$quantiles) ) stop("Please provide values or quantiles.")

    if(is.function(covlist$ref) || !is.null(covlist$quantiles)){
        if(missing(data)) stop("using functions to derive reference values and/or selecting covariate values by quantiles requires a data set to be passed in the data argument.")
        setDT(data)
        }
    
    ## handle ref if a function
    if(is.function(covlist$ref)){
        if(!covlist$covvar %in% colnames(data)){
            stop(sprintf("Covariate %s is not a column in data.",covlist$covvar))
        }
        pars.id <- findCovs(data,by=col.id,as.fun="data.table")
        if(!covlist$covvar %in% colnames(pars.id)){
            stop(sprintf("Covariate %s is not constant within %s in data.",covlist$covvar,col.id))
        }

        covlist$ref <- signif(pars.id[,covlist$ref(get(covlist$covvar))],digits=sigdigs)

    }

    
    
    ## add values from quantiles when quantiles are provided
    if(!is.null(covlist$quantiles)){
        if(!covlist$covvar %in% colnames(data)){
            stop(sprintf("Covariate %s is not a column in data.",covlist$covvar))
        }
        pars.id <- findCovs(data,by=col.id,as.fun="data.table")
        if(!covlist$covvar %in% colnames(pars.id)){
            stop(sprintf("Covariate %s is not constant within %s in data.",covlist$covvar,col.id))
        }
        
        covlist$values <- signif(pars.id[,quantile(get(covlist$covvar),probs=covlist$quantiles,names=FALSE)], digits=sigdigs)
        
### todo carry over names
                if(!is.null(names(covlist$quantiles))){
                    names(covlist$values) <- names(covlist$quantiles)
                }
    }


    
    ## if(!is.null(covlist$quantiles)){
    ##     if(is.null(names(covlist$quantiles))){
    ##         covlist$covvalc <- covlist$values
    ##     } else {
    ##         covlist$covvalc <- names(covlist$quantiles)
    ##     }
    ## }

    covlist$covvalc <- NA_character_
    covlist$refc <- NA_character_
    if(!is.null(covlist$values)) {
        if(is.null(names(covlist$values))){
            covlist$covvalc <- covlist$values
        } else {
            covlist$covvalc <- names(covlist$values)
        }
        if(is.null(names(covlist$ref))){
            covlist$refc <- covlist$ref
        } else {
            covlist$refc <- names(covlist$ref)
        }
    }
    

    

    ## fill in label if missing
    if(is.null(covlist$label)) covlist$label <- paste(covlist$covvar)
    
    ## make data.table
    with(covlist,
         data.table(covvar=covvar,covval=c(values,ref),covvalc=c(covvalc,refc),covlabel=label,covref=ref,type=c(rep("value",length(values)),"ref"))
         )
    
    
}
