##' Create or update $SIZES in a control stream
##'
##' Update $SIZES parameters in a control stream. The control stream
##' can be in a file or provided as a character vector (file lines).
##' 
##' @param file.mod A path to a control stream. See also alternative
##'     `lines` argument. Notice, if `write` is `TRUE` (default) and
##'     `newfile` is not provided, `file.mod` will be overwritten.
##' @param newfile An optional path to write the resulting control
##'     stream to. If nothing is provided, the default is to overwrite
##'     `file.mod`.
##' @param lines Control stream lines as a character vector. If you
##'     already read the control stream - say using
##'     `NMdata::NMreadSection()`, use this to modify the text lines.
##' @param wipe The default behavior (`wipe=FALSE`) is to add the
##'     `$SIZES` values to any existing values found. If SIZES
##'     parameter names are overlapping with existing, the values will
##'     be updated. If `wipe=TRUE`, any existing `$SIZES` section is
##'     disregarded.
##' @param write Write results to `newfile`?
##' @param warn Warn that this function is deprecated?
##' @param ... The $SIZES parameters. Provided anything, like `PD=40`
##'     See examples.
##' @return Character lines with updated control stream
##' @examples
##' ## No existing SIZES in control stream
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr032.mod",package="NMsim")
##' NMupdateSizes(file.mod,LTV=50,write=FALSE)
##' }
##' ## This controls stream has existing SIZES
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr134.mod",package="NMsim")
##' NMupdateSizes(file.mod,LTV=50,write=FALSE)
##' }
##' ## provide control stream as text lines
##' \dontrun{
##' file.mod <- system.file("examples/nonmem/xgxr032.mod",package="NMsim")
##' lines <- readLines(file.mod)
##' NMupdateSizes(lines=lines,LTV=50,write=FALSE)
##' }
##' @import NMdata
##' @importFrom utils modifyList
##' @export

NMupdateSizes <- function(file.mod=NULL,newfile,lines=NULL,wipe=FALSE,write=!is.null(newfile),warn=TRUE,...){
    
### only exported in 0.1.6 and 0.2.0. Deprecated starting from 0.2.1.
    if(warn){
        .Deprecated(new="NMdata::NMwriteSizes()")    
    }

    if(packageVersion("NMdata")<"0.1.8.905"){
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
        sizes.old <- NMreadSizes(file.mod=file.mod,lines=lines)
    }

### combine old and new sizes. modifyList prioritizes last list:
    ## modifyList(list(a=1,b=1),list(b=2,c=2))
    if(!is.null(sizes.old)){
        sizes.new <- modifyList(sizes.old,sizes.new)
    }
    
    ## convert sizes.new into text lines for control stream
    lines.new <- paste(c(
        "$SIZES",
        paste(names(sizes.new),sizes.new,sep="=")
    ),collapse=" "
    )
    
                                        # create lines from file.mod if not passed in as 'lines'
    if(!is.null(file.mod) && is.null(lines)) {
        lines <- readLines(file.mod,warn=FALSE)
    } 
    
                                        # if model already has $SIZES and we want to overwrite or append to it.
    if(!is.null(sizes.old)) {
                                        # if we want to overwrite it or append to it, sizes.new and lines.new will already be modified for that, so replace
        textlines = NMdata:::NMwriteSectionOne(lines = lines,section="SIZES",newlines=lines.new,location="replace")

                                        # else if model does not have $SIZES, create it
    } else if (is.null(NMreadSizes(lines=lines))) {
        textlines = NMdata:::NMwriteSectionOne(lines=lines,section="SIZES",newlines=lines.new,location="first")
        
                                        # else if it had $SIZES but we wiped it, and we want to replace/append it:
    } else if (!is.null(NMreadSizes(lines=lines)) && wipe) {
        textlines = NMdata:::NMwriteSectionOne(lines=lines,section="SIZES",newlines=lines.new,location="replace")
    }
    
    if(write && !is.null(newfile)) {
        writeTextFile(textlines, newfile)
    } else {
        return(textlines)
    }

}

##' Read SIZES info from a control stream
##' @param file.mod Control stream path.
##' @param lines Character vector with control stream file.
##' @import NMdata
##' @keywords internal


NMreadSizes <- function(file.mod=NULL,lines=NULL){
    
    lines.sizes <- NMreadSection(file=file.mod,lines=lines,section="SIZES",keep.empty=F,keep.comments=FALSE,keep.name=FALSE)
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
