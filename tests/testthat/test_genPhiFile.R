context("genPhiFile")

library(data.table)
data.table::setDTthreads(1) 


NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/genPhiFile_01.rds"
    etas <- data.table(ID=1:2,ETA1=c(.01,-.2))
    file.phi <- "testOutput/genPhiFile_out_01.phi"
    genPhiFile(etas,file=file.phi)
    readLines(file.phi)
    res <- NMreadPhi(file.phi)
    expect_equal_to_reference(res,fileRef)

})
