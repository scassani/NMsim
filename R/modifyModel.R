##' Internal method for handling modify.model argument to NMsim
##' @param modify.model A list
##' @param dt.models a data.table
##' @keywords internal



modifyModel <- function(modify.model,dt.models){

    ##
    modifyModelOne <- function(modify.model,dt.models){
        . <- NULL
        path.sim <- NULL
        ROWMODEL <- NULL
        
        ## grab name and other arguments for NMwriteSection. 
        if(is.list(modify.model[[1]])){
            args <- modify.model[[1]]
            args$section <- toupper(names(modify.model))
            args$quiet <- TRUE
            args$backup <- FALSE
            ## do not use list.sections
        } else {
            args <- list(quiet=TRUE,backup=FALSE,list.sections=modify.model)
        }
        dt.models[,{
            args.all= append(list(files=path.sim),args)
            do.call(NMwriteSection,args.all)
        },by=.(ROWMODEL)]

    }

    for(Nmodif in 1:length(modify.model)){
        modifyModelOne(modify.model[Nmodif],dt.models)
    }

    dt.models
}
