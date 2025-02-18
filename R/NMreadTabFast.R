### read using fread, then merge with input data. Assume NMREP exists.

## file.mod <- "~/tmp/sims/xgxr032_tabopts5/xgxr032_tabopts5.lst"
## NMreadTabFast(file.mod=file.mod)

##' @import data.table
##' @import NMdata

NMreadTabFast <- function(file,file.mod,carry.out,col.row=NULL,...){

    if(missing(file.mod)||is.null(file.mod)){
        file.mod <- file
    }
    if(missing(carry.out) || is.null(carry.out)){
        carry.out <- TRUE
    }
    
    meta.output <- NMscanTables(file=file,quiet=TRUE,as.fun="data.table",skip.absent=TRUE,
                                modelname=file.mod,col.model="model",meta.only=TRUE)

    
    alltabs <- lapply(meta.output$file,fread,...)

    cbind.new <- function(x,y){
        cbind(x,y[,setdiff(colnames(y),colnames(x)),with=FALSE])
    }

    alltabs <- Reduce(cbind.new,alltabs)
    

    ## add input data
    if(!isFALSE(carry.out)){

        inp <- NMscanInput(file=file,file.mod=file.mod,apply.filters=FALSE,translate=TRUE,recover.cols=TRUE,as.fun="data.table")
        if(isTRUE(carry.out)) carry.out <- setdiff(colnames(inp),colnames(alltabs))

        

        if(is.null(col.row)){
            
            col.by <- NULL
            if("NMREP"%in%colnames(alltabs)) col.by <- "NMREP"
            ## alltabs <- alltabs[,cbind(inp[,carry.out,with=FALSE],.SD),keyby=col.by,.SDcols=setdiff(colnames(alltabs),c(colnames(inp),col.by))]
            alltabs <- alltabs[,cbind(inp[,carry.out,with=FALSE],.SD),keyby=col.by,
                               .SDcols=setdiff(colnames(alltabs),c(carry.out,col.by))]
            setkey(alltabs,NULL)
        } else {
            alltabs[,.tmpcol:=.I]
            alltabs <- merge(alltabs,inp[,unique(c(col.row,carry.out)),with=FALSE],by=col.row,all.x=TRUE)
            setorder(alltabs,.tmpcol)
            alltabs[,.tmpcol:=NULL]

        }
    }
    
    alltabs
}
