##' file.mod <- system.file("examples/nonmem/xgxr032.mod",package="NMsim")
##' NMupdateSizes(file.mod,LTV=50,write=FALSE)
##' ## This controls stream has existing SIZES 
##' file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMsim")
##' NMupdateSizes(file.mod,LTV=50,write=FALSE)
##' ## provide control stream as text lines
##' file.mod <- system.file("examples/nonmem/xgxr032.mod",package="NMsim")
##' lines <- readLines(file.mod)
##' NMupdateSizes(lines=lines,LTV=50,write=FALSE)


context("NMupdateSizes")
library(data.table)
data.table::setDTthreads(1) 
library(NMdata)

NMdataConf(reset=T)
test_that("Basic",{
    fileRef <- "testReference/NMupdateSizes_01.rds"

    file.mod <- "testData/nonmem/xgxr032.mod"

    if(packageVersion("NMdata")>"0.1.8.904"){
        res <- NMupdateSizes(file.mod,LTV=50,write=FALSE)

        expect_equal_to_reference(res,fileRef)

        if(F){
            ref <- readRDS(fileRef)
            dt.res <- data.table(text=res)[,line:=.I]
            dt.ref <- data.table(text=ref)[,line:=.I]

            dt.all <- merge(dt.res,dt.ref,by="line")
            names(dt.all)
            dt.all[,text.x==text.y]

            opts <- options(widt=200)
            print(dt.all)
            options(opts)
        }
        
    }
    
})


test_that("with exisiting $SIZES",{
    fileRef <- "testReference/NMupdateSizes_02.rds"

    file.mod <- "testData/nonmem/xgxr051.mod"

    if(packageVersion("NMdata")>"0.1.8.904"){
        res <- NMupdateSizes(file.mod,LTV=50,write=FALSE)

        expect_equal_to_reference(res,fileRef)
    }
    
})
