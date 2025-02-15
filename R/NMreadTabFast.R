### read using fread, then merge with input data. Assume NMREP exists.

## file.mod <- "~/tmp/sims/xgxr032_tabopts5/xgxr032_tabopts5.lst"
## NMreadTabFast(file.mod=file.mod)

##' @import data.table
##' @import NMdata

NMreadTabFast <- function(file.mod,carry.out,...){
    
    if(missing(carry.out) || is.null(carry.out)){
        carry.out <- TRUE
    }

    meta.output <- NMscanTables(file=file.mod,quiet=TRUE,as.fun="data.table",skip.absent=TRUE,
                                modelname=file.mod,col.model="model",meta.only=TRUE)

    
    alltabs <- lapply(meta.output$file,fread,...)

    cbind.new <- function(x,y){
        cbind(x,y[,setdiff(colnames(y),colnames(x)),with=FALSE])
    }

    alltabs <- Reduce(cbind.new,alltabs)


    ## add input data
    if(!isFALSE(carry.out)){

        inp <- NMscanInput(file.mod,apply.filters=TRUE,translate=TRUE,recover.cols=TRUE,as.fun="data.table")
        if(isTRUE(carry.out)) carry.out <- setdiff(colnames(inp),colnames(alltabs))

        
        ## out1[,row.sub:=1:.N,by="NMREP"]
        ## out1[,row:=1:.N]
        ## dt.sim[,row.sub:=1:.N]
        ## res1 <- merge(out1,dt.sim,by="row.sub")
        ## setorder(res1,row)
        
        ## res1[,(c("row","row.sub")):=NULL]
        col.by <- NULL
        if("NMREP"%in%colnames(alltabs)) col.by <- "NMREP"
        ## alltabs <- alltabs[,cbind(inp[,carry.out,with=FALSE],.SD),keyby=col.by,.SDcols=setdiff(colnames(alltabs),c(colnames(inp),col.by))]
        alltabs <- alltabs[,cbind(inp[,carry.out,with=FALSE],.SD),keyby=col.by,
                           .SDcols=setdiff(colnames(alltabs),c(carry.out,col.by))]
        setkey(alltabs,NULL)
    }
    
    alltabs
}
