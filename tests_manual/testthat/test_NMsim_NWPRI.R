## install.packages("NMsim",repos="https://cloud.r-project.org")
library(data.table)
library(NMdata)

packageVersion("NMdata")
## library(NMsim)
library(devtools)
load_all(export_all=FALSE)

NMdataConf(reset=TRUE)
NMdataConf(dir.psn=NULL)
NMdataConf(as.fun="data.table")
NMdataConf(dir.sims="testOutput/simtmp")
NMdataConf(dir.res="testOutput/simres")

dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(data=dt.dos,TIME=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]


## NMdataConf(dir.psn="/opt/psn")
path.nonmem <- "/opt/nonmem/nm751/run/nmfe75"
## dir.psn <- "/opt/psn"
## NMdataConf(dir.psn=dir.psn)
##
path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75"
file.exists(path.nonmem)

#### need a function to drop NMsimVersion and NMsimTime from table
fix.time <- function(x){
    meta.x <- attr(x,"NMsimModTab")
    ## meta.x$time.call <- as.POSIXct("2020-02-01 00:01:01",tz="UTC")
    meta.x$NMsimVersion <- NULL
    meta.x$NMsimTime <- NULL
    
    setattr(x,"NMsimModTab",meta.x)
    invisible(x)
}


#### get rid of ROWMODEL2. Not needed for NMreadSim.


context("NMsim")
test_that("basic - default",{
    
    fileRef <- "testReference/NMsim_NWPRI_01.rds"

    ## 025 doesn't seem stable. Got Q~1e7 and Nonmem didn't run
    file.mod <- "testData/nonmem/xgxr233.mod"
    ## NMdata:::NMreadExt( fnExtension(file.mod,"ext"))
    ## library(nonmem2R)
    ## extload(file.mod)

    dt.sim[,DOSE2:=50]
    
    set.seed(43)
    simres <- NMsim(file.mod,
                    data=dt.sim,
                    table.var="PRED IPRED",
                    name.sim="NWPRI_01",
                    method.sim=NMsim_NWPRI
                    )
    
    ## attributes(NMreadSim("testOutput/NMsim_xgxr021_default_01_paths.rds"))
    fix.time(simres)
    ## expect_equal_to_reference(simres[,!("sim")],fileRef)
    expect_equal_to_reference(simres,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        compareCols(simres,ref,keep.names=TRUE)
        compareCols(attributes(simres)$NMsimModTab,attributes(readRDS(fileRef))$NMsimModTab,keep.names=FALSE)
        simres.nometa <- copy(simres)
        unNMsimRes(simres.nometa)
        attributes(simres.nometa)
        expect_equal_to_reference(simres.nometa,fnAppend(fileRef,"noMeta"))
    }

})

