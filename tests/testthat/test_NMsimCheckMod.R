context("NMsimCheckMod")

test_that("basic",{

    
    expect_null( NMsim:::NMsimCheckMod(file.mod="testData/nonmem/xgxr021.mod") )

})


test_that("using lines",{

    ## from xgxr021.mod
    lines <- "$ERROR
  IPRED=F
  IRES=DV-IPRED

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGMA(1,1) + SIGMA(2,2))
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=F+F*ERR(1)+ERR(2)
"

    expect_null( NMsim:::NMsimCheckMod(lines=lines ))

})


test_that("OBS ONLY",{

    ## from Ahmed
    lines <- "$ERROR (OBS ONLY)
        IPRED   = F
        W       = SQRT(ADD**2+PROP**2*IPRED**2)
        DEL     = 0
        IF(W.EQ.0) DEL = 1
        IRES    = DV-IPRED
        IWRES   = IRES/(W+DEL)
        Y       = IPRED + W*EPS(1)"


    expect_warning(
        NMsim:::NMsimCheckMod(lines=lines )
    )

})


test_that("OBSERVATIONS ONLY",{

    ## made up for OBSERVATIONS ONLY
    lines <- "$ERROR (OBSERVATIONS ONLY)
        IPRED   = F
        W       = SQRT(ADD**2+PROP**2*IPRED**2)
        Y       = IPRED + W*EPS(1)"

    expect_warning(
        NMsim:::NMsimCheckMod(lines=lines )
    )

})
