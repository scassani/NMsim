context("NMsim_VarCov.R")

library(data.table)
library(NMdata)
NMdataConf(reset=TRUE)

### dir.sims may be different in these tests than in other test
### scripts. The reason is the control streams are the outputs to be
### compared.
NMdataConf(
    dir.sims="testOutput"
    ## dir.sims="testOutput/simtmp"
    ##   ,dir.res="testOutput/simres"
)


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


test_that("Basic",{

        fileRef <- "testReference/NMsim_VarCov_01.rds"
        
        file.mod <- "testData/nonmem/xgxr032.mod"

        
        sim1 <- NMsim(file.mod=file.mod,
                      data=dat.sim,
                      name.sim = "VarCov_1",
                      method.sim=NMsim_VarCov,
                      nsims=2,
                      seed.R=2342,
                      execute=FALSE,
                      method.update.inits="nmsim")

        mod <- NMreadSection("testOutput/xgxr032_VarCov_1/xgxr032_VarCov_1_2.mod")
        
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

        }


})
