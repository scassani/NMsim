context("samplePars_mvrnorm")

if(F){
    test_that("mvrnorm through samplePars()",{
        file.mod <- "testData/nonmem/xgxr021.mod"

        fileRef <- "testReference/samplePars_mvrnorm_01.rds"
        
        pars.mvrnorm <- samplePars(file.mod=file.mod,nsims=10,method="mvrnorm",format="ext",as.fun="data.table",seed.R=23)

        cols.round <- c("value","value.est")
        pars.mvrnorm[,(cols.round):=lapply(.SD,round,3),.SDcols=cols.round]
        expect_equal_to_reference(pars.mvrnorm,fileRef)
        
    })


    test_that("nsims=1",{
        file.mod <- "testData/nonmem/xgxr021.mod"

        fileRef <- "testReference/samplePars_mvrnorm_02.rds"
        
        pars.1 <- samplePars(file.mod=file.mod,nsims=1,method="mvrnorm",format="ext",as.fun="data.table",seed.R=23)

        pars.2 <- samplePars(file.mod=file.mod,nsims=2,method="mvrnorm",format="ext",as.fun="data.table",seed.R=23)

        res <- compareCols(pars.1,pars.2)
        
        expect_equal_to_reference(res,fileRef)
        ## readRDS(fileRef)
        
    })

    test_that("truncation",{

        file.mod <- "testData/nonmem/xgxr033.mod"

        fileRef <- "testReference/samplePars_mvrnorm_03.rds"
        
        pars <- samplePars(file.mod=file.mod,nsims=1000,method="mvrnorm",format="ext",as.fun="data.table",seed.R=23)
        dt.ranges <- pars[,as.list(range(value)),by=parameter]

        expect_equal_to_reference(dt.ranges,fileRef)

    })
}
