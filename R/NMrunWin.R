cleaningPatterns <- function(clean){
    if(! clean %in% 1:4){
        stop ("only clean values 1, 2, 3, and 4 are supported")
    }
    c("nonmem","worker*","FDATA*","fort.*","WK_*")
}

##' Internal function to run Nonmem on linux
##' @param fn.mod Just the file name, not including path
##' @keywords internal
NMrunLin <- function(fn.mod,dir.mod.abs,exts.cp,meta.tables,path.nonmem,clean,sge,nc,pnm){

    name <- NULL
    fn.lst <- fnExtension( fn.mod,".lst")
    
    if(sge){
        ## executing from model execution dir.
        jobname <- sub(pattern="^ *",replacement="", x=basename(fn.mod) )
        ## qsub does not allow a jobname to start in a numeric
        if(grepl("^[0-9]",jobname)) {
            jobname <- paste0("NMsim_",jobname)
        }
        
        line.run <- sprintf('qsub -pe orte %s -V -N \"%s\" -j y -cwd -b y \"%s\" -background -parafile=%s [nodes]=%s'
                           ,
                            nc
                           ,
                            jobname
                           ,
                            ## paste(path.nonmem,basename(fn.mod),fnExtension( basename(fn.mod),".lst"),"; echo DONE > NMexec_complete.txt")
                            paste(path.nonmem,fn.mod,fn.lst)
                              ##,basename(pnm)
                             ,getAbsolutePath(pnm)
                             ,nc
                              )
        
        ## line.run <- sprintf("qsub -pe orte %s -V -N %s %s %s",
        ##                     nc
        ##                    ,jobname
        ##                     path.nonmem,fn.mod,fnExtension(fn.mod,".lst"))
        
    } else {
        line.run <- sprintf("%s %s %s",path.nonmem,fn.mod,fn.lst)
    }

    lines.bash <- c(
        "#!/bin/bash"
        ,""
       ,line.run
       ,"stat=$?"
       ,"if [ $stat -ne 0 ]; then"
       ,"exit $stat;"
       ,"fi"
        ,sprintf("until [ -f %s ]; do",fn.lst)
        ,"sleep 2"
       ,"done"
       ## ,sprintf("( tail -f -n4 %s & ) | grep -q \"Stop Time:\"",fn.lst)
       ## ,"echo Stop found"
         ,sprintf("while ! grep -q \"Stop Time\" %s ; do",fn.lst)
       ,"sleep 2"
         ,"done"
### copy output tables back
        ## ,sprintf("cp \'%s\' \'%s\'",paste(meta.tables[,name],collapse="' '"),dir.mod.abs)
       ,paste0("find . -type f -name ",paste0("\'",meta.tables[,name],"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")
### copy wanted files back to orig location of fn.mod 
       ,paste0("find . -type f -name ",paste0("\'*.",exts.cp,"\'")," -exec cp {} \'",dir.mod.abs,"\' \\;")
       ,""
    )

    rm.if.pres <- function(regex){
        sprintf("find . -type f -name \"%s\" -exec rm {} \\;",regex)     
    }

    if(clean>0 && clean<5){

        patterns.clean <- cleaningPatterns(clean)
        lines.bash <- c(lines.bash
                       ,paste(unlist(lapply(patterns.clean,
                                            rm.if.pres)),collapse="\n")
                        )
        
    }
    if(clean==5){
        lines.bash <- c(lines.bash,
                        sprintf("cd ..; rm -r \"%s\"",dir.mod.abs)
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
        lines.bat <- c(lines.bat,
                       sprintf("CD .. & rd /s /q \"%s\"",dir.mod.abs)
                       )
    }
    
    lines.bat
}

