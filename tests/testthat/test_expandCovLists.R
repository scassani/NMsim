context("expandCovLists")

test_that("basic",{

    fileRef <- "testReference/expandCovLists_01.rds"
    file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
    modres <- NMdata::NMscanData(file.mod)
    
    res <- expandCovLists(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"
                 ),
        data=modres
    )

    expect_equal_to_reference(res,fileRef)
    
})
