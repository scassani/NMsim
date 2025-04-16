##' Generate system command to call Nonmem directly
##' @keywords internal

callNonmemDirect <- function(file.mod,path.nonmem){
    bfile.mod <- basename(file.mod)
    sprintf("cd %s; %s %s %s; cd -",dirname(file.mod),path.nonmem,bfile.mod,fnExtension(bfile.mod,".lst"))
}
