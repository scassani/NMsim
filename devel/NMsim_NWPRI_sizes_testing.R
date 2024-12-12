#### testing NMsim::NMsim_NWPRI after accounting for $SIZES LVR and LTH

unloadNamespace("NMsim")
unloadNamespace("NMdata")
library(devtools)
library(here)
library(scales)
library(tidyverse)
library(stringr)


### lets try to make the directories in the script only depend on wdirs. And maybe one more for setwd if needed
wdirs <- "/data/sandbox/trunk/analysis/NMsim/wdirs"
setwd("/data/sandbox/trunk/analysis/NMsim/wdirs")
# wdirs <- "~/wdirs"
load_all(file.path(wdirs,"NMdata"))
load_all(file.path(wdirs,"NMsim"))


NMdataConf(path.nonmem="/data/sandbox/trunk/analysis/NMsim/nonmem_pre_releases/nm760/run/nmfe76",
           as.fun="data.table")
# NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75",
#            as.fun="data.table")


printSection <- function(section) {
  if(!is.list(section)) section <- list(section)
  res <- lapply(section,function(x){
    x <- x[!grepl("^ *$",section)]
    cat(paste(paste(x,collapse="\n"),"\n\n"))
    
  })
}

##### Choose a model

### The model shown in the NMsim-ParamUncertain vignette
file.project <- function(...)file.path(system.file("examples",package="NMsim"),...)
file.mod <- file.project("nonmem/xgxr032.mod")
## dat.sim <- read_fst(here::here("wdirs/NMsim/vignettes/simulate-results/dat_sim.fst"))
data.sim <- read_fst(file.path(wdirs,"NMsim/vignettes/simulate-results/dat_sim.fst"))


pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[value!=0]
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

NMsim(
  file.mod=file.mod,              ## Path to estimation input control stream
  data=data.sim                    ## simulation input data
  ,dir.sims="NMsim/devel/simtmp_NWPRI_nm76"
  # ,modify.model = list(
  #   ## name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare
  #   ## the distributions to other methods.
  #   ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
  # )  
  # ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )   
  ,method.sim=NMsim_default
  , execute = FALSE
  ## Var-Cov parameter sampling
  ,name.sim="NWPRI_sizes"               ## a recognizable directory name
  ,subproblems=250                 ## sampling multiple models
  ,sge=FALSE                      ## run simulations in parallel please
  ,nmquiet=T
  ,reuse.results=F
)

file.sim = "/data/sandbox/trunk/analysis/NMsim/wdirs/NMsim/devel/simtmp_NWPRI_nm76/xgxr032_NWPRI_sizes/xgxr032_NWPRI_sizes.mod"

# a file with no SIZES line
lines = readLines(file.sim)
NMsim:::NMreadSizes(file.mod = file.sim) # works
NMsim::NMupdateSizes(lines =  lines, LTV = 50) # works
NMsim::NMupdateSizes(file.mod=file.sim, LTV = 50, write=F) # works

NMsim::NMupdateSizes(file.mod = file.sim, LTV=50) #works
NMsim::NMupdateSizes(file.mod = file.sim, newfile = fnAppend(file.sim, "updateSize"), LTV=5000) # works

# a file with SIZES
lines = c("$SIZES LIM1=10000 LTH=40 LVR=20", lines)
NMsim::NMupdateSizes(lines = lines, LTH=50) # works


# 2822
file.mod = "/data/prod_vx548_phase3_analysis/trunk/analysis/NDA/models/PK/2822/2822.mod"
data.sim = NMdata::NMscanInput(file.mod,translate = T,recover.cols = FALSE, apply.filters = TRUE) %>% 
  # dplyr::filter(USUBJIDN==10001) %>% 
  # dplyr::select(USUBJIDN:WTBLI) %>% 
  rename(ID=USUBJIDN, TIME=AFRLT) %>% 
  filter(ID %in% sample(ID, size = 10))

pars <- NMreadExt(file.mod,return="pars",as.fun="data.table")
pars[, parlab := stringr::str_remove_all(string = parameter, pattern = "\\(|\\)") %>% stringr::str_replace(",", "\\_")]

# # first generate the empty sim control stream:
NMsim::NMsim(
    file.mod=file.mod,              ## Path to estimation input control stream
    data=data.sim                    ## simulation input data
    ,dir.sims="NMsim/devel/simtmp_NWPRI_nm76" ## where to store temporary simulation files
    ,modify.model = list(
        ## name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare
        ## the distributions to other methods.
        ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
    )
    ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )
    ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
    , execute = FALSE
    ,name.sim="NWPRI_sizes"               ## a recognizable directory name
    ,subproblems=250                 ## sampling multiple models
    ,sge=FALSE                      ## run simulations in parallel please
    ,nmquiet=F
    ,reuse.results=F
)
file.sim="NMsim/devel/simtmp_NWPRI_nm76/2822_NWPRI_sizes/2822_NWPRI_sizes.mod"

# check if model looks right.
readLines(file.sim)


NMsim::NMsim(
  file.mod=file.mod,              ## Path to estimation input control stream
  data=data.sim                    ## simulation input data
  ,dir.sims="NMsim/devel/simtmp_NWPRI_nm76" ## where to store temporary simulation files
  ##   ,dir.res="simulate-results"      ## where to store simulation results files
  ,modify.model = list(
    ## name the THETAS/OMEGAS/SIGMAS in $ERROR so we can compare
    ## the distributions to other methods.
    ERROR = add( paste0(pars$parlab, " = ", pars$par.name) )
  )
  ,table.vars = paste0( "PRED IPRED Y ", paste0(pars$parlab, collapse = " ") )
  ,method.sim=NMsim_NWPRI         ## Var-Cov parameter sampling
  ,name.sim="NWPRI_sizes"               ## a recognizable directory name
  ,subproblems=50                 ## sampling multiple models
  ,sge=FALSE                      ## run simulations in parallel please
  ,nmquiet=F
  ,reuse.results=F
  ,wait = T 
)

 

