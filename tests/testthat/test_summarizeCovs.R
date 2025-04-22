context("summarizeCovs")
NMdataConf(reset=TRUE)


if(FALSE){

    ## file.mod <- "NMsim-forest-models/xgxr134.mod"
    file.mod <- "testData/nonmem/xgxr134.mod"
    doses <- NMcreateDoses(TIME=0,AMT=30,ADDL=100,II=24,col.id=NA)

    data.ref <- NMdata::NMscanData(file.mod,quiet=TRUE)
    covs <- expandCovs(
        AGE=list(ref=median,quantiles=c(10,25,75,90)/100,label="Age (years)"),
        ## notice, values OR quantiles can be provided
        WEIGHTB=list(ref=median, quantiles=c(10,25,75,90)/100, label="Bodyweigt (kg)"),
        MALEN=list(ref=c(Female=0), values=c(Male=1), label="Sex"),
        data=data.ref,
        as.fun="data.table"
    )
    ## adding distinct ID's for each combination of covariates
    covs[,ID:=.GRP,by=.(type,covvar,covval)]
    covs


    dt.dos <- covs[,doses[],by=covs]
    dims(covs,doses,dt.dos)


    time.sim <- rbind(
        data.table(TIME=seq(0,24,.25),period="Day 1"),
        data.table(TIME=seq(0,24,.25)+30*24,period="Steady-State")
    )
    dt.sim <- addEVID2(dt.dos,TIME=time.sim,CMT=2)
    dims(dt.dos,time.sim,dt.sim)

    simres <- NMsim(file.mod # path to NONMEM model
                   ,data=dt.sim, # simulation dataset
                   ,dir.sims="testOutput/simtmp",
                   ,dir.res="testData/simres",
                   ,name.sim="forest1" # output name suffix
                   ,method.sim=NMsim_NWPRI # sampling with mvrnorm
                   ,subproblems=3
                   ,typical=TRUE # FALSE to include BSV
                   ,table.vars=cc(PRED,IPRED) # output table variables
                   ,seed.R=342 # seed for reproducibility
                    )
}


test_that("basic",{

    fileRef <- "testReference/summarizeCovs_01.rds"
    
    simres <- NMreadSim("testData/simres/xgxr134_forest1_MetaData.rds")

    funs.exposure <- list(
        "Cmax"=function(x) max(x$IPRED)
        ## ,"AUC"=function(x) trapez(x$TIME,x$IPRED)
        ## ,"Concentration at 4 hours"=function(x) x$value[x$TAPD==4]
    )
    
    res <- summarizeCovs(simres,
                         funs.exposure = funs.exposure,
                         by=cc(period),
                         cover.ci=.95)
    
    expect_equal_to_reference(res,fileRef)
    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }

})


test_that("zero refs",{
    
    fileRef <- "testReference/summarizeCovs_02.rds"
    ##    simres <- NMreadSim("testOutput/xgxr134_forest1_MetaData.rds")
    simres <- NMreadSim("testData/simres/xgxr134_forest1_MetaData.rds")
    
    funs.exposure <- list(
        "Cmax"=function(x) max(x$IPRED),
        "Cmin"=function(x) min(x$IPRED)
        ## ,"AUC"=function(x) trapez(x$TIME,x$IPRED)
        ## ,"Concentration at 4 hours"=function(x) x$value[x$TAPD==4]
    )

    res <-
        expect_warning(
            summarizeCovs(simres,
                          funs.exposure = funs.exposure,
                          by=cc(period),
                          cover.ci=.95
                          )
        )


    expect_equal_to_reference(res,fileRef)
    if(FALSE){
        ref <- readRDS(fileRef)
        ref
        res
    }


})
