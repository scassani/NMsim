##' Add class if not already present
##' @param data The object to add class to
##' @param class The class to add (character)
##' @keywords internal
##' @return Object with additional class
## don't export

addClass <- function(data,class){
    allclasses <- class(data)
    if(!class %in% allclasses) allclasses <- c(class,allclasses)
    setattr(data,"class",allclasses)
}
