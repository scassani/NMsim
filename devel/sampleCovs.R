### todo
## check if covs are already present in data.sim.1subj

## chekc if ther is only one subject in data.sim.1subj

## support non-dt

## check if covs are present in data.covs

## handle by args
set.seed(2372)
Nsubjs <- 100
col.id.data <- "USUBJID"
col.id <- "ID"
data.covs <- readRDS("../../pkpd_programming/phase1_poppk/outputs/VX993_PH1_POPPK_all_01Apr2025.rds")
data.sim.1subj <- copy(dt.sim)
covs <- "WTBL"

### calcs
dt.covs <- data.covs  |>
    findCovs(by=c(col.id.data))
dt.covs <- dt.covs[,c(col.id.data,covs),with=FALSE]
setnames(dt.covs,col.id.data,"IDCOVS")
dt.ids <- data.table(ID=1:Nsubjs)
setnames(dt.ids,"ID",col.id)
dt.ids[,IDCOVS:=sample(dt.covs[,IDCOVS],size=.N,replace=T)]
dt.ids <- mergeCheck(dt.ids,dt.covs,by="IDCOVS")

dt.sim.covs <- dt.ids[,
                      dt.sim[,setdiff(colnames(dt.sim),c(col.id,covs)),with=FALSE]
                     ,by=dt.ids]
setorder(dt.sim.covs,ID,TIME,EVID)

## return dt.sim.covs
