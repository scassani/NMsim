##' Writes a parameter table to a control stream
##'
##' @param values A list of lists. Each list specifies a parameter
##'     with named elements. par.type and i are required. j is
##'     required for OMEGA and SIGMA. value, ll, ul and fix can be
##'     supplied to modify the parameter. See examples.
##'
##' @examples
##' values=list( list(par.type="THETA",i=2,value=1.4),
##'              list(par.type="THETA",i=3,FIX=1),
##'              list(par.type="OMEGA",i=2,j=2,value=0.1))

#### ext should not be mandatory. If not supplied, just make changes based on control stream.

## list(type="omega",i=3,j=3,init=4)
NMwriteInits <- function(file.mod,update=TRUE,file.ext=NULL,values,newfile,...){
    
    
    if(missing(values)) values <- NULL
    dots <- list(...)
    values <- append(values,dots)
    
    mod <- NMreadSection(file.mod)
    thetas.mod <- NMreadCtlPars(mod$THETA,section="THETA")
    omegas.mod <- NMreadCtlPars(mod$OMEGA,section="OMEGA")
    sigmas.mod <- NMreadCtlPars(mod$SIGMA,section="SIGMA")

### not sure if the dcasting should be before or after updating
    pars.l <- rbind(thetas.mod,omegas.mod,sigmas.mod)
    
    if(is.null(file.ext)) file.ext <- file.mod


    
############## write  parameter sections
    ## need to write line by line. All elements in a line written one at a time
    
    ## dont dcast. Stickto one elem per row. But I think we must create a new element typu ll,init,ul. Because the current string.elem has all three elements written. 

    paste.ll.init.ul <- function(ll,init,ul,FIX){
        if(any(is.na(init))) stop("An initial value must be provided")
        if(any(!is.na(ul)&is.na(ll))) stop("if upper limit is provided, lower limit must also be provided.")
        dt <- data.table(ll=ll,init=init,ul=ul)[,row:=.I]
        dt[init=="SAME",res:=init]
        dt[init!="SAME",res:=paste0("(",paste(setdiff(c(ll,init,ul),NA),collapse=","),")",FIX),by=row]
        dt[init!="SAME"&is.na(ll)&is.na(ul),res:=paste0(init,FIX),by=row]
        dt$res
    }
    ## reduce ll, init and ul lines to just ll.init.ul lines
### for  this approach, dcast, then paste.ll...
    ## this is complicated. Better make paste function operate on long format.
    
######### Limitation: ll, init, and ul must be on same line
    pars.l[type.elem=="FIX",value.elem:=fifelse(value.elem=="1"," FIX","")]
    inits.w <- dcast(
        pars.l[type.elem%in%c("ll","init","ul","FIX")]
       ,par.type+linenum+parnum+i+j+iblock+blocksize~type.elem,value.var=c("elemnum","value.elem"),funs.aggregate=min)

### the rest of the code is dependent on all of init, ll, and ul being available.
    cols.miss <- setdiff(outer(c("value.elem","elemnum"),c("init","ll","ul","FIX"),FUN=paste,sep="_"),colnames(inits.w))
    inits.w[,(cols.miss):=NA_character_]
    ##    inits.w[,fix:=ifelse(FIX=="1","FIX","")]
    inits.w[is.na(value.elem_FIX),value.elem_FIX:=""]
    
############ update paramters
    inits.w[,modified:=0]
### update from ext
    if(update){
        
        ext <- NMreadExt(file.ext,as.fun="data.table")
        ## thetas.ext <- ext[par.type=="THETA"]
        ext
        inits.w <- mergeCheck(inits.w[,-("value.elem_init")],ext[,.(par.type,i,j,value.elem_init=as.character(value))],by=c("par.type","i","j"),all.x=TRUE,fun.na.by=NULL,quiet=TRUE)
    }
    
### Implement changes as requested in values
    fun.update.vals <- function(dt,value,name){
        
        names(value) <- tolower(names(value))

        name <- gsub(" +","",name)
        name <- tolower(name)
        par.type <- sub("^([a-z]+)\\(.*","\\1",name)

        if(par.type=="theta"){
            i <- as.integer(sub(paste0(par.type,"\\(([0-9]+)\\)"),"\\1",name))
            j <- NA
        }

        if(par.type%in%c("omega","sigma")){
            i <- as.integer(sub(paste0(par.type,"\\(([0-9]+),([0-9]+)\\)"),"\\1",name))
            j <- as.integer(sub(paste0(par.type,"\\(([0-9]+),([0-9]+)\\)"),"\\2",name))
        }


        if(F){
            if(!all(c("par.type","i")%in% names(value))){
                stop("value must contain `par.type` and `i`")
            }
            if(!value$par.type %in% c("THETA","OMEGA","SIGMA")){
                stop("`par.type` must be one of `THETA`,`OMEGA`, and `SIGMA`.")
            }
            if(value$par.type %in% c("OMEGA","SIGMA") && ! "j" %in% names(value)){
                stop("For `par.type` one of `OMEGA` and `SIGMA`, `j` must be provided.")
            }
            if(!"j" %in% names(value)) {
                value$j <- NA
            }
        }
        
        if("fix" %in% names(value)) {
            if(value$fix) {
                value$fix <- " FIX"
            } else {
                value$fix <- ""
            }
        }
        
        ## value.values <- value[setdiff(names(value),c("par.type","i","j"))]
        value.values <- value
        names.vals <- names(value.values)
        names.vals[names.vals=="fix"] <- "FIX"
        names.vals[names.vals%in%c("init","ll","ul","FIX")] <- paste0("value.elem_",names.vals[names.vals%in%c("init","ll","ul","FIX")])
        names(value.values) <- names.vals
        ## make sure FIX is "" or " FIX"

        value$par.type <- par.type
        value$i <- i
        value$j <- j
        
        if(value$par.type=="THETA"){
            dt[par.type==value$par.type & i==value$i, (names(value.values)):=value.values]
        } else {
            dt[par.type==value$par.type & i==value$i & j==value$j, (names(value.values)):=value.values]
        }
        dt
    }

    
    names.values <- names(values)
    if(!is.null(values)){
        for(I in 1:length(values)){
            inits.w <- fun.update.vals(inits.w,values[[I]],names.values[I])
        }
    }

    
    
######### format paramters for ctl
    inits.w[,type.elem:="ll.init.ul"]
    inits.w[,row:=1:.N]
    
    inits.w[,string.elem:=paste.ll.init.ul(value.elem_ll,value.elem_init,value.elem_ul,value.elem_FIX),by=row]
    inits.w[,elemnum:=min(elemnum_ll,elemnum_init,elemnum_ul,na.rm=TRUE),by=row]

    cnames.common <- intersect(colnames(pars.l),colnames(inits.w))
    elems.all <- rbind(
        pars.l[!type.elem%in%c("ll","init","ul","FIX")][,cnames.common,with=FALSE]
       ,
        inits.w[,cnames.common,with=FALSE]
    )

    elems.all <- elems.all[order(par.type,linenum,elemnum)]
    elems.all[,row:=.I]
    ## idx.update <- elems.all[par.type%in%c("OMEGA","SIGMA"), row[1], by = .(par.type,iblock)][,V1]
    idx.update <- elems.all[, row[1], by = .(par.type,iblock)][,V1]
    elems.all[idx.update, string.elem := paste(paste0("$",par.type),string.elem)]
    lines.all <- elems.all[,.(text=paste(string.elem,collapse=" ")),keyby=.(par.type,linenum)]

    

    lines.new <- readLines(file.mod)

    fun.update.ctl <- function(lines.old,section,dt.lines){
        newsection <- dt.lines[par.type==section,text]
        if(length(newsection)==0) return(lines.old)
        
        NMwriteSectionOne(lines=lines.old,
                          section=section,
                          newlines=newsection,
                          location="replace")
    }

    lines.new <- fun.update.ctl(lines.new,section="THETA",dt.lines=lines.all)
    lines.new <- fun.update.ctl(lines.new,section="OMEGA",dt.lines=lines.all)
    lines.new <- fun.update.ctl(lines.new,section="SIGMA",dt.lines=lines.all)
    
    lines.new <- NMwriteSectionOne(lines=lines.new,
                                   section="THETA",
                                   newlines=lines.all[par.type=="THETA",text],
                                   location="replace")


    if(!is.null(newfile)){
        writeTextFile(lines.new,newfile)
        return(invisible(lines.new))
    }
    
    lines.new
    
}
