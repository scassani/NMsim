library(devtools)
load_all()

library(NMdata)
NMdataConf(as.fun="data.table")
library(data.table)

dt.dos <- NMcreateDoses(AMT=300,TIME=0)
dt.sim <- addEVID2(data=dt.dos,TIME=c(1,6,12),CMT=2)
dt.sim[,BBW:=40][,ROW:=.I]

dt.sim.known <- egdt(dt.sim[,!("ID")],data.table(ID=101:105))
setorder(dt.sim.known,ID,TIME,EVID,CMT)

#### We want NMsim to run without PSN. Tell NMsim where to find the
#### Nonmem executable.
## NMdataConf(dir.psn="/opt/psn")
NMdataConf(path.nonmem ="C:/nm75g64/run/nmfe75.bat")
NMdataConf(dir.psn=NULL)



file.mod <- "../tests/testthat/testData/nonmem/xgxr021.mod"


set.seed(43)
## Not specifying method.execute - PSN should not be used if
## NMdataConf was used to specify Nonmem path.
simres <- NMsim(file.mod,
                data=dt.sim,
                table.vars="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
                )

## use PSN (path to folder containing "execute" executable may need to be specified above.
simres <- NMsim(file.mod,
                data=dt.sim,
                table.vars="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
               ,method.execute="psn"
                )

## Explicitly ask to use Nonmem directly
simres <- NMsim(file.mod,
                data=dt.sim,
                table.vars="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
               ,method.execute="NMsim"
                )

## not specifying method.update.inits - default is using PSN if found
simres <- NMsim(file.mod,
                data=dt.sim,
                table.vars="PRED IPRED",
                dir.sims="testOutput",
                name.sim="default_01"
                )

### next step is to test an example with method.sim=NMsim_known



