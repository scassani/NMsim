cleaningPatterns <- function(clean){
    if(! clean %in% 1:4){
        stop ("only clean values 1, 2, 3, and 4 are supported")
    }
    c("nonmem","worker*","FDATA*","fort.*","WK_*","temp_dir","FSUBS*")
}

##' Internal function to run Nonmem on linux
##' @param fn.mod Just the file name, not including path
##' @keywords internal
NMrunLin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem,clean,sge,nc,pnm){

    rm.if.pres <- function(regex){
        sprintf("find . -type f -name \"%s\" -exec rm {} \\;",regex)     
    }


    name <- NULL
    fn.lst <- fnExtension( fn.mod,".lst")
    line.run <- sprintf("%s %s %s",path.nonmem,fn.mod,fn.lst)

    if(sge){
        ## executing from model execution dir.
        jobname <- 
            sub(pattern="^ *",replacement="", x=basename(fn.mod) )
        ## qsub does not allow a jobname to start in a numeric
        if(grepl("^[0-9]",jobname)) {
            jobname <- paste0("NMsim_",jobname)
        }
        line.run <- sprintf('qsub -terse -pe orte %s -V  -e \"%s\" -o \"%s\" -N \"%s\" -j y -cwd -b y \"%s\" -background'
                           ,nc
                           ,"NMexec.err"
                           ,"NMexec.out"
                           ,jobname
                           ,line.run
                            )

        if(nc>1){
            line.run <- sprintf('%s -parafile=\"%s\" [nodes]=%s',line.run,getAbsolutePath(pnm),nc)
        }
        ##line.run <- paste(line.run,"| read jobid_qsub")
        line.run <- paste0("jobid_qsub=$(",line.run,"  2>&1)")
    }

    if(F){
        lines.wait <- c("RUNNING=1"
                        ## ,sprintf("until [ -f %s ]; do",fn.lst)
                       ,"until [ $RUNNING -eq 0 ]; do"
                       ,"sleep 2"
                       ,sprintf("if [ -f %s ] ; then
  RUNNING=0
fi",fn.lst)
,"done"
,sprintf("if [ -f %s ] ; then
  txterr=`grep \" ERROR \" %s`
  if [ \"$txterr\" != \"\" ] ; then 
    RUNNING=0 
    echo \"NMTRAN error found\"
    exit 1;
  fi
fi",
file.path("FMSG"),
file.path("FMSG"))

## ,sprintf("( tail -f -n4 %s & ) | grep -q \"Stop Time:\"",fn.lst)
## ,"echo Stop found"

,sprintf("while ! grep -q \"Stop Time\" %s ; do",fn.lst)
,"sleep 2"
,"done")
    }

    lines.wait <- c()
    if(sge) {
        path.wait.qsub <- system.file("bash/wait_qsub.sh",package="NMsim")
        lines.wait <- paste(path.wait.qsub, "\"$jobid_qsub\"")
    }
    
    lines.bash <- c(
        "#!/bin/bash"
       ,""
       ,line.run
       ,lines.wait
### copy output tables back
### this would be simpler. Needs testing.
        ## ,sprintf("find . -type f -name \'%s\' -exec cp {} \'%s\' \\;",meta.tables[,name],dir.mod.abs)
       ,paste0("find . -type f -name ",paste0("\'",meta.tables[,name],"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")

### copy wanted files back to orig location of fn.mod 
       ,paste0("find . -type f -name ",paste0("\'*.",exts.cp,"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")
       ,""
    )

    if(clean>0 && clean<5){

        patterns.clean <- cleaningPatterns(clean)
        lines.bash <- c(lines.bash
                       ,paste(unlist(lapply(patterns.clean,
                                            rm.if.pres)),collapse="\n")
                        )
        
    }
    if(clean==5){
        lines.bash <- c(lines.bash
                       ,"oldwd=$PWD"
                       ,"cd .."
                       ,"rm -r \"$oldwd\""
                       ,""
                        )
    }
    lines.bash
}


##sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
NMrunWin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem,clean){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ##     path.nonmem <- FALSE
    name <- FALSE

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    cp.if.pres <- function(fn,dest){
        sprintf("IF EXIST \"%s\" COPY /Y \"%s\" \"%s\"",fn,fn,dest)
    }
    rm.if.pres <- function(fn){
        sprintf("IF EXIST \"%s\" DEL \"%s\"",fn,fn)
    }

    lines.bat <- c(
        sprintf("call %s %s %s",path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
       ,
        paste(unlist(lapply(fnExtension(fn.mod,exts.cp),cp.if.pres,dest=dir.mod.abs)),collapse="\n")
       ,
        paste(unlist(lapply(meta.tables[,name],cp.if.pres,dest=dir.mod.abs)),collapse="\n")
    )

### Cleaning
    if(clean>0 && clean<5){
        lines.bat <- c(lines.bat,
                       paste(unlist(lapply(cleaningPatterns(clean),
                                           rm.if.pres)),collapse="\n")
                       )
    }

    if(clean==5){

        lines.bat <- c(lines.bat
                       ,"set \"oldwd=%cd%\""
                       ,"CD .."
                       ##sprintf("CD .. & rd /s /q \"%s\"",dir.tmp)
                       ,"rd /s /q \"%oldwd%\""
                       )
    }
    
    lines.bat
}

