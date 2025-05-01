### todo
## check if covs are already present in data.sim.1subj

## chekc if ther is only one subject in data.sim.1subj

## support non-dt

## check if covs are present in data.covs

## handle by args
## data.covs <- readRDS("/data/prod_vx993_phase1_analysis/trunk/pkpd_programming/phase1_poppk/outputs/VX993_PH1_POPPK_all_01Apr2025.rds") |>
## setDT()
data.covs <- NMscanData(system.file("examples/nonmem/xgxr114.mod",package="NMsim"))

data.sim.1 <- NMcreateDoses(TIME=0,AMT=100) |>
    NMaddSamples(TIME=c(1,4),CMT=2)

sampleCovs(data=data.sim.1,Nsubjs=3,col.id.data="USUBJID",data.covs=data.covs,covs=c("SEXN","WTBL"))


sampleCovs <- function(data,
                       Nsubjs,
                       col.id.data = "ID",
                       col.id= "ID",
                       data.covs ,
                       covs ,
                       seed.R,
                       as.fun
                       ){

    data <- as.data.table(data)
    
    if(missing(seed.R)) seed.R <- NULL
    if(!is.null(seed.R)) set.seed(seed.R)

    if(missing(covs)) covs <- NULL
    if(is.null(covs)) {
        message("No covariates requested.")
    }

    if(missing(Nsubjs)) Nsubjs <- NULL
    if(is.null(Nsubjs)) {
        stop("Nsubjs must be supplied.")
    }

    ## as.fun
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdata:::NMdataDecideOption("as.fun",as.fun)

    
### calcs
    dt.covs <- data.covs  |>
        findCovs(by=c(col.id.data),as.fun="data.table")
    dt.covs <- dt.covs[,c(col.id.data,covs),with=FALSE]
    setnames(dt.covs,col.id.data,"IDCOVS")
    dt.ids <- data.table(ID=1:Nsubjs)
    setnames(dt.ids,"ID",col.id)
    dt.ids[,IDCOVS:=sample(dt.covs[,IDCOVS],size=.N,replace=TRUE)]
    dt.ids <- mergeCheck(dt.ids,dt.covs,by="IDCOVS",as.fun="data.table")

    dt.sim.covs <- dt.ids[,
                          data[,setdiff(colnames(data),c(col.id,covs)),with=FALSE]
                         ,by=dt.ids]
    setorder(dt.sim.covs,ID,TIME,EVID)

    ## return dt.sim.covs
    as.fun(dt.sim.covs)
}
