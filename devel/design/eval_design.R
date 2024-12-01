library(NMdata)
library(devtools)

wdirs <- "~/wdirs"

dir.here <- file.path(wdirs,"NMsim/devel/design")

load_all(file.path(wdirs,"NMsim"),export_all=FALSE)


NMdataConf(
    path.nonmem="/opt/nonmem/nm751/run/nmfe75",
    dir.sims=file.path(dir.here,"simtmp"),
    dir.res=file.path(dir.here,"simres"),
    as.fun="data.table"
)


file.mod <- file.path(file.path(wdirs,"NMsim/tests/testthat/testData/nonmem/xgxr032.mod"))

dt.dos <- NMcreateDoses(TIME=0,CMT=1,AMT=40)
dt.sim <- addEVID2(dt.dos,TIME=c(1,10,24),CMT=2)

dt.sim[,TMIN:=.1]


### NMsim by default inserts a $SIMULATION
## replace $SIMULATION with $DESIGN
NMsim(file.mod=file.mod,
      data=dt.sim,
      modify.model=list(sim="$DESIGN options...")
      )

## replace $SIMULATION with $DESIGN. Drop $TABLE
NMsim(file.mod=file.mod,
      data=dt.sim,
      modify.model=list(sim="$DESIGN options...",
## I think this will remove the $TABLE section 
                        table=""
                        )
      )

### With parameter uncertainty. Notice, NMsim_NWPRI can fail if thetas
### are fixed, and "later" thetas are non-fixed. If this is an issue,
### look at sampleParsSimpar().
NMsim(file.mod=file.mod,
      data=dt.sim,
      method.sim=NMsim_NWPRI,
      modify.model=list(sim="$DESIGN options...")
      )

### we can still not fix selected thetas during model run.
