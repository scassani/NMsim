
##' Create or update $SIZES in a control stream
##' @param
##' @examples
##' file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMsim")
##'
##' NMupdateSizes(file.mod,newfile="~/newmod.mod",LTV=50)
##' @importFrom utils modifyList


NMupdateSizes <- function(file.mod,newfile=file.mod,wipe=FALSE,...){

    if(packageVersion("NMdata")<"0.1.8.904"){
        stop("NMupdateSizes requires NMdata 0.1.9 or later.")
    }
    
    sizes.new <- list(...)
### check sizes.new
    ## all elements must be named.
    ## Names must not contain spaces.
    ## all elements numeric?

    if(wipe){
        sizes.old <- NULL
    } else {
        sizes.old <- NMreadSizes(file.mod)
    }

### combine old and new sizes. modifyList prioritizes last list:
    ## modifyList(list(a=1,b=1),list(b=2,c=2))
    sizes.new <- modifyList(sizes.old,sizes.new)
    
    ## convert sizes.new into text lines for control stream
    lines.new <- paste(c(
        "$SIZES",
        paste(names(sizes.new),sizes.new,sep="=")
    ),collapse=" "
    )
    
    if(!is.null(sizes.old)){
        NMwriteSection(files=file.mod,newfile=newfile,section="SIZES",newlines="",location="replace")
    } else if(newfile!=file.mod){
        file.copy(file.mod,to=newfile)
    }
    NMwriteSection(files=newfile,newfile=newfile,section="SIZES",newlines=lines.new,location="first")

}

##' file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMsim")
##'
##' NMreadSizes(file.mod)


NMreadSizes <- function(file.mod){
    lines.sizes <- NMreadSection(file=file.mod,section="SIZES",keep.empty=F,keep.comments=FALSE,keep.name=FALSE)
    if(is.null(lines.sizes)) return(NULL)
    
    ## make it just one line
    lines.sizes <- paste(lines.sizes,collapse=" ")
    ## Replace all " +" by " " (multiple spaces to just one space)
    ## lines.sizes <- gsub(" +"," ",lines.sizes)
    ## clean the found SIZES first to make sure " *=" and "= *" are replaced by "=".
    ## lines.sizes <- gsub(" *= *","=",lines.sizes)
    lines.sizes <- NMdata:::cleanSpaces(lines.sizes)
    ## Then strsplit with " " as separator should identify parameters and values.
    strings.sizes <- strsplit(lines.sizes,split=" ")[[1]]
    ## We now have strings "par1=val1" and "par2=val2". These have to be separated - could be done by strsplit again, this time using = as separator.
    list.sizes.strings <- lapply(strings.sizes,function(x)strsplit(x,split="=")[[1]])
    list.sizes.lists <- lapply(list.sizes.strings,function(x)setNames(list(x[2]),x[1]))
    list.sizes <- do.call(c,list.sizes.lists)

    list.sizes
}
