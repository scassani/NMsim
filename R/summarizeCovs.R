##' Summarize simulated exposures relative to reference subject
##' @param data Simulated data to process. This data.frame must
##'     contain must contain multiple columns, as defined by
##'     NMsim::expandCovs().
##' @param funs.exposure A named list of functions to apply for
##'     derivation of exposure metrics.
##' @param cols.value The default is to run exposure metrics based on
##'     the `PRED` column. Specify another or multiple columns to run
##'     the analysis on.
##' @param cover.ci The coverage of the confidence intervals. Default
##'     is 0.95.
##' @param by a character vector of names columns to perform all
##'     calculations by. This could be sampling subsets or analyte.
##' @param as.fun The default is to return data as a
##'     `data.frame`. Pass a function (say `tibble::as_tibble`) in
##'     as.fun to convert to something else. If data.tables are
##'     wanted, use `as.fun="data.table"`. The default can be
##'     configured using `NMdataConf()`.
##' @details These columns are expected to be present, and differences within any of them will lead to separate summarizing (say for a s covariate value to be plotted):
##' cc(model,type,pred.type,covvar,covlabel,covref,covval)
##'
##' @importFrom stats median reorder setNames
##' @return A data.frame
##' @export



summarizeCovs <- function(data,funs.exposure,cols.value,cover.ci=0.95,by,as.fun){

    . <- NULL
    EVID <- NULL
    covlabel <- NULL
    covref <- NULL
    covval <- NULL
    covvalc <- NULL
    covvalf <- NULL
    covvar <- NULL
    median <- NULL
    metric.val <- NULL
    model <- NULL
    pred.type <- NULL
    predm <- NULL
    reorder <- NULL
    run.sim <- NULL
    setNames  <- NULL
    type <- NULL
    val.exp.ref <- NULL
    

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)
    
    ## A standard evaluation interface to data.table::dcast
    dcastSe <- function(data,l,r,...){
        lhs <- paste(l,collapse="+")
        formula.char <- paste(lhs,r,sep="~")
        dcast(data,formula=as.formula(formula.char),...)
    }
    
### this is using model and model.sim as introduced in NMsim 0.1.4
    ## will not work with earlier versions!

    ## predefined data columns to calculate by
    databy <- cc(model,type,pred.type,covvar,covlabel,covref,covval,covvalc)
    
    ## allby expands "by" to contain data columns that calculations
    ## will always be done by.
    allby <- c(databy,by)

### modelby are NMsim model columns that will be used if found
    ## model.sim should always be present starting from NMsim 0.1.4. NMREP
    ## is in case a method using SUBPROBLEMS is used - like NWPRI.
    modelby <- intersect(c("model.sim","NMREP"),colnames(data))

    
    simres <- as.data.table(data)[EVID==2]

    if(missing(cols.value)) cols.value <- NULL
    if(is.null(cols.value)){
        simres[,pred.type:=NA]
    } else {
### The var.conc argument applied
        ## long format so calculations can be done by "prediction type".
        simres <- melt(simres,
                       measure.vars=cols.value,
                       variable.name="pred.type",
                       value.name="value")
    }

### summarizing exposure metrics for each subject in each model,
### each combination of covariates
    resp.model <- simres[,lapply(funs.exposure,function(f)f(.SD)),
                         by=c(allby,modelby,"ID")]
    
    
### the exposure metrics in long format.
    mvars <- names(funs.exposure)
    resp.model.l <- melt(resp.model,measure.vars=mvars,variable.name="metric.var",value.name="metric.val")

    
    ## deriving median by model and time to have a single value per model
    ## and time point. This is only needed in case multiple subjects are
    ## simulated by each model.
    sum.res.model <- resp.model.l[
       ,.(predm=median(metric.val))
       ,by=c(modelby,allby,"metric.var")
    ]

    
### making reference value a column rather than rows. 
    ## column with refrence exposure value is called val.exp.ref
    dt.ref <- setnames(
        sum.res.model[type=="ref",c(modelby,"metric.var",setdiff(allby,c("covval","covvalc","type")),"predm"),with=FALSE]
       ,"predm","val.exp.ref")
    ## these columns are not necessarily in refr columns. If not, drop them before merge.
    dt.miss <- dt.ref[,lapply(.SD,function(x)all(is.na(x))),.SDcols=c("covvar","covlabel","covref")]
    cols.miss <- colnames(dt.miss[,as.logical(colSums(dt.miss)),with=FALSE])
    if(length(cols.miss)){
        dt.ref <- dt.ref[,!(cols.miss),with=FALSE]
    }
    
    sum.res.model <- mergeCheck(sum.res.model[type=="value"],
                                dt.ref
                               ,
                                by=c(modelby,"metric.var",setdiff(allby,c("covval","covvalc","type",cols.miss))),
                                fun.na.by=NULL,
                                quiet=TRUE
                                )

    
### summarize distribution of ratio to ref across parameter samples/models
    sum.uncertain <- sum.res.model[
       ,setNames(as.list(quantile(predm/val.exp.ref,probs=c((1-cover.ci)/2,.5,1-(1-cover.ci)/2))),
                 c("predml","predmm","predmu"))
       ,by=c(allby,"metric.var")]


### Section end: Summarize exposure metrics vs covariates
    ## A factor representation of covariate values - only based on the remainding values - not reference
    sum.uncertain[,covvalf:=reorder(covvalc,covval)]

    as.fun(sum.uncertain)
}
