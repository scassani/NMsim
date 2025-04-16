context("addEVID2")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/addEVID2_01.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,time.sim=seq.time,CMT=2)

    df.doses
    
    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }
    
})


test_that("Multiple compartments",{
    fileRef <- "testReference/addEVID2_02.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=c(2,3))

    ## dt.doses
    ## res
    ## readRDS(fileRef)
    expect_equal_to_reference(res,fileRef)
    
    if(F){
        res
        readRDS(fileRef)
    }
    
})



test_that("compartments with covariates",{
    fileRef <- "testReference/addEVID2_03.rds"

    dt.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    seq.time <- c(0,4,12,24)
    dt.cmt <- data.table(CMT=c(2,3),analyte=c("parent","metabolite"))
    
    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=dt.cmt)

    ## res
    ## readRDS(fileRef)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

})



test_that("data.frame CMT",{
    fileRef <- "testReference/addEVID2_04.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1,as.fun=as.data.frame)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,time.sim=seq.time,CMT=c(2,3),as.fun=as.data.frame)

    df.doses
    
    expect_equal_to_reference(res,fileRef)
    if(F){
        res
        readRDS(fileRef)
    }

})


test_that("time with covariates",{
    fileRef <- "testReference/addEVID2_05.rds"

    dt.doses <- NMcreateDoses(TIME=data.table(regimen=c("SD","MD","MD"),TIME=c(0,0,12)),AMT=10,CMT=1)

    seq.time.sd <- data.table(regimen="SD",TIME=seq(0,6))
    seq.time.md <- data.table(regimen="MD",TIME=c(0,4,12,24))
    seq.time <- rbind(seq.time.sd,seq.time.md)
    
    res <- addEVID2(dt.doses,time.sim=seq.time,CMT=2)

    ## dt.doses
    ## res
    
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("EVID=0",{
    fileRef <- "testReference/addEVID2_06.rds"

    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=2)
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,time.sim=seq.time,CMT=2,EVID=0)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
    
})


test_that("time after dose",{
    fileRef <- "testReference/addEVID2_07.rds"
    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    df.doses
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,TAPD=seq.time,CMT=2)
    res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})


test_that("TIME and TAPD",{
    fileRef <- "testReference/addEVID2_08.rds"
    df.doses <- NMcreateDoses(TIME=c(0,12),AMT=10,CMT=1)
    df.doses
    seq.time <- c(0,4,12,24)

    res <- addEVID2(df.doses,TIME=seq.time,TAPD=3,CMT=2)
    res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("TAPD with ADDL/II",{
    fileRef <- "testReference/addEVID2_09.rds"
    df.doses <- NMcreateDoses(TIME=c(0),ADDL=2,II=12,AMT=10,CMT=1)

    seq.time <- c(0,2,4,18,24)

    ## res0 is when doses are expanded in advance
    df.doses.0 <- NMexpandDoses(df.doses)
    
    res0 <- addEVID2(df.doses.0,TAPD=seq.time,CMT=2)
    
    res1 <- addEVID2(df.doses,TAPD=seq.time,CMT=2)

    expect_equal_to_reference(res1,fileRef)

    expect_equal(as.data.table(res0)[EVID==2],
                 as.data.table(res1)[EVID==2])
    
    
    if(F){
        res
        readRDS(fileRef)
    }


})

test_that("simple, more than one id",{
    NMdataConf(as.fun="data.table")
    fileRef <- "testReference/addEVID2_10.rds"
    dt.dos <- NMcreateDoses(TIME=c(0), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos[,grp:=NULL]

    res <- addEVID2(dt.dos,TAPD=c(1),CMT=2)

    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }

})




test_that("TAPD -  covariates on dosing data, not on TAPD",{
    fileRef <- "testReference/addEVID2_11.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- addEVID2(dt.dos,TAPD=c(1),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("TAPD -  covariates on dosing data, and on TAPD",{
### 
    ## this is replicating too much
    fileRef <- "testReference/addEVID2_12.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    ## not all subjects covered
    res <- addEVID2(dt.dos,TAPD=data.frame(grp="a",TAPD=1),CMT=2)
    expect_equal_to_reference(res,fileRef)
})

test_that("all subjects covered",{
### 
    fileRef <- "testReference/addEVID2_13.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    res <- addEVID2(dt.dos,TAPD=data.frame(grp=c("a","b"),TAPD=c(1,2)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})

test_that("sampling schemes overlapping into next doses",{
    fileRef <- "testReference/addEVID2_14.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- addEVID2(dt.dos,TAPD=data.frame(TAPD=c(1,13)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("sampling at time of doses",{
    fileRef <- "testReference/addEVID2_15.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- addEVID2(dt.dos,TAPD=data.frame(TAPD=c(1,12)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})


test_that("sampling times not unique",{
    fileRef <- "testReference/addEVID2_16.rds"
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res <- addEVID2(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2)
    res
    expect_equal_to_reference(res,fileRef)
})

test_that("No CMT column",{

    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=NA) 
    res <- addEVID2(dt.dos,TAPD=c(1,1))

    expect_false("CMT"%in%colnames(res))

    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]))
    expect_error(addEVID2(dt.dos,TAPD=c(1,1)))
    
})

test_that("Providing DV",{
    NMdataConf(reset=TRUE)
    dt.dos <- NMcreateDoses(TIME=c(0,12), AMT=data.table(AMT=1:2,grp=letters[1:2]),CMT=1) 
    dt.dos
    res0 <- addEVID2(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2,DV=0,as.fun="data.table")
    res2 <- addEVID2(dt.dos,TAPD=data.frame(TAPD=c(1,1)),CMT=2,as.fun="data.table")
    res0
    res2
    expect_equal(
        transform(res0,EVID=NULL,MDV=1,DV=NULL)
       ,
        transform(res2,EVID=NULL)
    )

    expect_equal(
        res0[,sort(unique(MDV))]
       ,c(0,1))

    expect_equal(
        res0[,sort(unique(DV))]
       ,c(0))

})

test_that("Unmatched covariates",{
    fileRef <- "testReference/addEVID2_17.rds"

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0, TMIN=0, TMAX=200),AMT=70,CMT=NA)

    res <- expect_message(
        NMsim::addEVID2(dt.dos,TIME = data.frame(TIME=c(1,4,8), TSTRAT=seq(1:length(samp.times)), 
                                                 TMIN=rep(0.01,length(samp.times)),
                                                 TMAX=rep(200,length(samp.times))))
    )


    expect_equal_to_reference(res,fileRef)
    
})


test_that("Unmatched covariates",{
    fileRef <- "testReference/addEVID2_17.rds"

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0, TMIN=0, TMAX=200),AMT=70,CMT=NA)

    dt.time <- data.frame(TIME=c(1,4,8), TSTRAT=c(1,2,3))

    res <- expect_message(
        NMsim::addEVID2(dt.dos,TIME = dt.time
                        ## data.frame(TIME=c(1,4,8), TSTRAT=seq(1:length(samp.times)), 
                        ##            TMIN=rep(0.01,length(samp.times)),
                        ##            TMAX=rep(200,length(samp.times)))
                        )
    )


    expect_equal_to_reference(res,fileRef)
    
})


test_that("Unmatched are not covs",{
    fileRef <- "testReference/addEVID2_18.rds"

    samp.times <- c(1, 4, 8)
    dt.dos <- NMsim::NMcreateDoses(TIME=data.frame(TIME=0, TSTRAT=0, TMIN=0, TMAX=200),AMT=70,CMT=NA)
    dt.time <- data.frame(TIME=c(1,4,8), TSTRAT=c(1,2,3))
    
    res <- NMsim::addEVID2(dt.dos,
                           TIME =dt.time
                          ,
                           extras.are.covs=FALSE)

    ## res
    expect_equal_to_reference(res,fileRef)

    if(F){
        res
        readRDS(fileRef)
    }
})

