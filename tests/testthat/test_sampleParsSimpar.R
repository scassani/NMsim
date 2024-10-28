context("sampleParsSimpar")

### waiting for NMdata 0.1.8 on CRAN
if(F){
    test_that("basic",{

        loadres <- requireNamespace("simpar",quietly=TRUE)
        if(!loadres) {
            ## message("simpar not available. Please install from github or MPN.")
            expect_true(TRUE)
        } else {

            fileRef <- "testReference/sampleParsSimpar_01.rds"

            file.mod <- "testData/nonmem/xgxr021.mod"
            pars <- sampleParsSimpar(file.mod,nsim=10,format="wide",as.fun="data.table",seed.R=23)

            expect_equal_to_reference(pars,fileRef)
        }
    })
}
