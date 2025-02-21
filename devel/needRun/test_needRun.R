

source("needRun.R")


## we need to run it inside a function to 
myfun2 <- function(a=1,b,file,...){

    
    path.res <- "myfun_res.rds"
    path.digests <- "myfun_digests.rds"

    ## should needRun be passed functions for "unwrapping"?
    ## I think the whole environment better be passed along
    run.fun <- needRun(path.res, path.digests)
    
    if(run.fun$needRun){
        res <- a+b
        ##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@28@"]]));##:ess-bp-end:##
        
        saveRDS(digests.fun,file=path.digests)
        saveRDS(res,path.res)
    } else {
        message("No changes. Reading results from previous run.")
        res <- readRDS(path.res)
    }
    res
}

myfun2(b=2,file="digest_all.R")



## we need to run it inside a function to 
myfun <- function(a=1,b,file,...){
    
    path.res <- "myfun_res.rds"
    path.digests <- "myfun_digests.rds"
    run.fun <- TRUE
    if(file.exists(path.digests)&&file.exists(path.res)){
        digests.old <- readRDS(path.digests)
        obj.fun <- callArgs() 
        digests.fun <- addCallRes(obj.fun,args=list(file=readLines))
        
        digests.all <- merge(digests.fun,digests.old,by="name",suffixes=c(".new",".old"),all.x=T,all.y=T)
        
        if(
            all(
                digests.all[,isTRUE(res.new==res.old),by=.(name)][,V1]
            )
           ){
            run.fun <- FALSE
        }
    }
    if(run.fun){
        res <- a+b
        saveRDS(digests.fun,file=path.digests)
        saveRDS(res,path.res)
    } else {
        message("No changes. Reading results from previous run.")
        res <- readRDS(path.res)
    }
    res
}

myfun(b=2,file="digest_all.R")
