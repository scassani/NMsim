context("NMreadParsWide")

NMdataConf(reset=TRUE)

if(F){
    file.mod <- "testData/nonmem/xgxr021.mod"
    pars <- sampleParsSimpar(file.mod,nsim=10,format="wide",as.fun="data.table",seed.R=23)
    saveRDS(pars,"testData/data/xgxr021_parsWide.rds")
}

test_that("basic",{

    fileRef <- "testReference/NMreadParsWide_01.rds"

    parsWide <- readRDS("testData/data/xgxr021_parsWide.rds")
    res <- readParsWide(parsWide)

    expect_equal_to_reference(res,fileRef)
    
})
