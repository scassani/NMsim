##' Parameter data from csv
##' 
##' Reads output table from simpar and returns a long format
##' data.table. This is the same format as returned by NMreadExt()
##' which can be used by NMsim. 
##'
##' @param data A data.frame or a path to a delimited file to be read
##'     using `data.table::fread`.
##' @param col.model Column containing name of the original model. By
##'     default a column called "model" will contain "Model1".
##' @param col.model.sim Name of the model counter, default is
##'     "model.sim". If the provided name is not found in data, it
##'     will be created as a row counter. Why needed? Each row in data
##'     represents a set of parameters, i.e. a model. In the long
##'     format result, each model will have multiple rows. Hence, a
##'     model identifier is needed to distinguish between models in
##'     results.
##' @param strings.par.type Defines how column names get associated
##'     with THETA, OMEGA, and SIGMA. Default is to look for "T", "O",
##'     or "S" as starting letter. If customizing, make sure each no
##'     column name will be matched by more than one criterion.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say \code{tibble::as_tibble}) in as.fun to convert
##'     to something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return a long-format data.frame of model parameters
##' @details The wide data format read by `readParsWide` is not a
##'     Nonmem format. It is used to bridge output from other tools
##'     such as simpar, and potentially PSN.
##'
##' This function reads a data that is "wide" in parameters - it has a
##'     column for each parameter, and one row per parameter set or
##'     "model". It returns a data set that is "long" in model and
##'     parameters. The long format contains
##' \itemize{
##' \item id.model.par The unique model-parameter identifier. The row-identifier.
##' \item model Model identifier. 
##' \item par.type ("THETA", "OMEGA", "SIGMA")
##' \item i and j indexes for the parameters (j is NA for par.type=="THETA").
##' \item value The parameter value 
##' \item parameter Nonmem-style parameter names. THETA1, OMEGA(1,1) etc. Notice the inconsistent naming of THETA vs others.
##' \item name.wide The column name in the wide data where this value was taken
##' }
##' The columns or "measure variables" from which to read values  are
##'     specified as three regular expressions, called THETA, OMEGA, and SIGMA. The default three regular expressions will associate a column name starting with "T" with THETAs, while "O" or "S" followed by anything means "OMEGA" or "SIGMA".
##'
##' readParsWide extracts i and j indexes from sequences of digits in the column names. TH.1 would be TETA1, SG1.1 is SIGMA(1,1).
##'
##' @import data.table
##' @import NMdata
##' @examples
##' \dontrun{
##' tab.ext <- readParsCsv("simpartab.csv")
##' ## or
##' tab.simpar <- fread("simpartab.csv")
##' tab.ext <- readParsCsv(tab.simpar)
##' NMsim(...,method.sim=NMsim_VarCov,tab.ext=tab.ext)
##' }
##' @export

readParsWide <- function(data,col.model,col.model.sim,strings.par.type=c(THETA="^T.*",OMEGA="^O.*",SIGMA="^S."),as.fun){


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    id.model.par <- NULL
    name.wide <- NULL
    value <- NULL
    . <- NULL
    par.type <- NULL
    parameter <- NULL
    model <- NULL
    i <- NULL
    j <- NULL


###  Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    
    if(is.data.frame(data)){
        data <- as.data.table(data)
    } else {
        data <- fread(data)
    }


    ## if col.model is null, create one
    if( missing(col.model) || is.null(col.model) ){
        col.model <- "model"
    }
    if( missing(col.model.sim) || is.null(col.model.sim)){
        col.model.sim <- "model.sim"
    }
    if(!col.model%in%colnames(data)){
        data[,(col.model):="Model1"]
    }
    cols.mod <- col.model
    if(col.model%in%colnames(data)){
        cols.mod <- c(col.model,cols.mod)
    }
    
    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    data[,(col.model.sim):=.I]
    
### convert to long format    
    pars.l <- melt(data,id.vars=c(col.model,col.model.sim),variable.name="name.wide",value.name="value")
    
    ## assign par.type
### id.model.par is unique across models x parameter
    ## dt.match is w
    dt.match <- pars.l[,lapply(strings.par.type,grepl,x=name.wide),by=c(col.model,col.model.sim,"name.wide")]
    dt.match.l <- melt(dt.match,id.vars=c(col.model,col.model.sim,"name.wide"),variable.name="par.type")[value==TRUE]

    ## TODO check that only one was matched
    
    ## assign parameter type. This could be done by merging a data.table with unique names and types instead of merging for all the sim models.
   
    pars.l <- mergeCheck(
        pars.l
       ,
        dt.match.l[,c(col.model.sim,"par.type","name.wide"),with=FALSE]
       ,by=c(col.model.sim,"name.wide"),quiet=TRUE)

    
    
    ##  extract the index numbers i,j from digits in the string
    deriveCols <- function(x,n){
        found <- regmatches(x,gregexpr("[1-9][0-9]*",x))
        
        found <- as.numeric(found[[1]])
        as.list(c(found,rep(NA,n-length(found))))
    }
    pars.l[,(c("i","j")):=deriveCols(name.wide,n=2),by=c(col.model,col.model.sim,"name.wide")]

    
    ## Assign Nonmem style name THETA1, OMEGA(1,1) etc. Notice the
    ## inconsistency in naming.
    NMparIdxNames <- function(dt){

        j <- NULL
        par.type <- NULL
        parameter <- NULL
        i <- NULL
        j <- NULL

        
        dt <- copy(dt)
        dt[,par.type:=toupper(par.type)]

        dt[par.type=="THETA",parameter:=paste0(par.type,i)]
        dt[par.type!="THETA",parameter:=sprintf("%s(%d,%d)",par.type,i,j)]

        dt[,parameter]
    }
    pars.l[,parameter:=NMparIdxNames(.SD)]
    ## if("id.model.par"%in%colnames(pars.l)) {
    ##     pars.l[,id.model.par:=NULL]
    ## }

    cols.order <- c(col.model,col.model.sim,cc(par.type,i,j,value,parameter,name.wide))
    cols.order <- intersect(cols.order,colnames(pars.l))
    setcolorder(pars.l,cols.order)

    as.fun(pars.l)

}
