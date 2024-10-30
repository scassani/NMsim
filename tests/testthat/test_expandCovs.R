context("expandCovLists")

if(F){
    test_that("basic",{

        fileRef <- "testReference/expandCovLists_01.rds"
        ## file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
        file.mod <- "testData/nonmem/xgxr134.mod"
        modres <- NMdata::NMscanData(file.mod)
        
        res <- expandCovLists(
            WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
            ## notice, values OR quantiles can be provided
            AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
            data=modres
        )

        expect_equal_to_reference(res,fileRef)
        
    })
}

test_that("with categorical",{

    fileRef <- "testReference/expandCovLists_02.rds"
    ## file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMdata")
    file.mod <- "testData/nonmem/xgxr134.mod"
    modres <- NMdata::NMscanData(file.mod)
    
    res <- expandCovLists(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        MALEN=list(ref=0,values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres
    )
    res

    expect_equal_to_reference(res,fileRef)
    
})

if(F){
    res <- expandCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        MALEN=list(ref=0,values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres
    )
    res

    res <- expandCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c(10,25,75,90)/100, label="Age (years)"),
        MALEN=list(ref=c(Female=0),values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres
    )


    res <- expandCovs(
        WEIGHTB=list(ref=70,values=c(40,60,80,100),label="Bodyweight (kg)"),
        ## notice, values OR quantiles can be provided
        AGE=list(ref=median, quantiles=c("10%"=10,"25%"=25,"75%"=75,"90%"=90)/100, label="Age (years)"),
        MALEN=list(ref=c(Female=0),values=c(Male = 1, Female = 0),label = "Sex"),
        data=modres
    )


    res

}
