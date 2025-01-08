context("NMwriteInits")

## ext <- NMreadExt(file.mod,as.fun="data.table")
## file.mod <- "/home/philip/wdirs/NMsim/devel/devel_writePars.mod"
## NMreadSection(file.mod,section="theta")



if(F){
    unloadNamespace("NMsim")
    unloadNamespace("NMdata")
    load_all("~/wdirs/NMdata")
    load_all("~/wdirs/NMsim")
}

test_that("Basic",{
if(packageVersion("NMdata")>"0.1.921"){
    
    fileRef <- "testReference/NMwriteInits_01.rds"
    
    file.mod <- "testData/nonmem/xgxr033.mod"

    ## NMreadSection(file.mod,section="OMEGA")
    ## NMreadCtlPars(readLines(file.mod),section="OMEGA")

    res <- list(
        NMwriteInits(file.mod,"THETA(1)"=list(init=3))
       ,
        ## unfix
        NMwriteInits(file.mod,"OMEGA(1,1)"=list(fix=0))

       ,
#### fix a block
        NMwriteInits(file.mod,"OMEGA(2,2)"=list(fix=1))
       ,
        NMwriteInits(file.mod,"OMEGA(2,2)"=list(init=1))

    )

    expect_equal_to_reference(res,fileRef)
}
})
