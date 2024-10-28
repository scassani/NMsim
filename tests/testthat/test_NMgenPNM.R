context("NMgenPNM")

test_that("basic",{

    fileRef <- "testReference/NMgenPNM_01.rds"

    fn <- tempfile()
    NMgenPNM(nc=16,file=fn)

    lines <- readLines(fn)

    expect_equal_to_reference(lines,fileRef)

})
