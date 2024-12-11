# library(devtools)
# library(testthat)
# withr::with_libpaths(new = file.path("../../../../renv/local/"), devtools::install_github("philipdelff/NMdata@v0.1.8" ))
# library("NMdata",lib.loc = "../../../../renv/local")
setwd("NMsim/tests/testthat")
devtools::load_all("../../../NMdata")
devtools::load_all("../../../NMsim")
context("NMsim_NWPRI.R")

library(data.table)
library(NMdata)
NMdataConf(reset=TRUE)


### so far disabled because test relies on NMdata 0.1.7

dt.amt <- data.table(DOSE=c(100,400))
dt.amt[,AMT:=DOSE*1000]
dt.amt
doses.sd <- NMcreateDoses(TIME=0,AMT=dt.amt,as.fun="data.table")
doses.sd[,dose:=paste(DOSE,"mg")]
doses.sd[,regimen:="SD"]


dat.sim.sd <- addEVID2(doses.sd,time.sim=0:24,CMT=2,as.fun="data.table")
dat.sim <- copy(dat.sim.sd)

## NMcheckData(dat.sim)

dat.sim[,ROW:=.I]

head(dat.sim)

dat.sim[,BBW:=75]


test_that("NMsim_NWPRI",{

    fileRef <- "testReference/NMsim_NWPRI_01.rds"
    
    file.mod <- "testData/nonmem/xgxr032.mod"

    
    sim1 <- NMsim(file.mod=file.mod,
                  data=dat.sim,
                  dir.sim="testOutput",
                  name.sim = "sd1_NWPRI",
                  method.sim=NMsim_NWPRI,
                  seed.nm=2342,
                  execute=FALSE,
                  method.update.inits="nmsim")

    mod <- NMreadSection("testOutput/xgxr032_sd1_NWPRI/xgxr032_sd1_NWPRI.mod")
    

    ## ref <- readRDS(fileRef)
    expect_equal_to_reference(mod$THETAPV,fnAppend(fileRef,"THETAPV"))
    expect_equal_to_reference(mod$OMEGAP,fnAppend(fileRef,"OMEGAP"))
    expect_equal_to_reference(mod,fileRef)

    if(F){
        ref <- readRDS(fileRef)
        ref$OMEGA
        mod$OMEGA 
        ref$SIGMA
        mod$SIGMA
        ref$SIMULATION
        mod$SIMULATION

compareCols(ref,mod)
        ref$SIZES
        mod$SIZES

        ref$OMEGAP
        mod$OMEGAP

        ref$THETAPV
        mod$THETAPV

        ref$INPUT
        mod$INPUT
    }


})

# block OMEGA 4x4
# will fail if NMdata version < 0.1.8.903
if(packageVersion("NMdata") >= "0.1.8.903") {
    
    test_that("NMsim_NWPRI_Omega44",{
        
        fileRef <- "testReference/NMsim_NWPRI_02.rds"
        
        file.mod <- "testData/nonmem/predu.ctl"
        dat.sim <- fread("testData/nonmem/example1.csv",skip = 1)
        
        sim1 <- NMsim(file.mod=file.mod,
                      data=dat.sim,
                      dir.sim="testOutput",
                      name.sim = "sd2_NWPRI",
                      method.sim=NMsim_NWPRI,
                      seed.nm=2342,
                      execute=FALSE,
                      method.update.inits="nmsim")
        
        mod <- NMreadSection("testOutput/predu_sd2_NWPRI/predu_sd2_NWPRI.mod")
        
        
        ## ref <- readRDS(fileRef)
        expect_equal_to_reference(mod$THETAPV,fnAppend(fileRef,"THETAPV"))
        expect_equal_to_reference(mod$OMEGAP,fnAppend(fileRef,"OMEGAP"))
        expect_equal_to_reference(mod,fileRef)
        
        if(F){
            ref <- readRDS(fileRef)
            ref$OMEGA
            mod$OMEGA 
            ref$SIGMA
            mod$SIGMA
            ref$SIMULATION
            mod$SIMULATION
            
            compareCols(ref,mod)
            
            ref$OMEGAP
            mod$OMEGAP
            
            ref$THETAPV
            mod$THETAPV
        }
        
        
    })
    
    
}


# block omega 2x2 mixed with other non block omegas
# xgxr033.mod
