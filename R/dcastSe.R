##' @importFrom stats as.formula
##' @keywords internal
## A standard evaluation interface to data.table::dcast
    dcastSe <- function(data,l,r,...){
        lhs <- paste(l,collapse="+")
        formula.char <- paste(lhs,r,sep="~")
        dcast(data,formula=as.formula(formula.char),...)
    }
