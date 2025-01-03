##' Create function that adds text elements to vector
##' @param ... Elements to add.
##' @param .pos Either \"top\" or \"bottom\". Decides if new text is prepended or appended to existing text.
##' @return A function that adds the specified text to character vectors
##' @examples
##' myfun <- add("b","d")
##' myfun("a")
##' myfun2 <- add("b","d",.pos="top")
##' myfun2("a")
##' @export
add <- function(...,.pos="bottom"){
    
    switch(.pos,
           top=function(x)c(unlist(list(...)),x),
           bottom=function(x)c(x,unlist(list(...)))
           )
    
}

##' Create function that modifies text elements in a vector
##' @param ... Passed to `gsub()`
##' @param fixed This is passed to gsub(), but `overwrite()`'s default behavior is the opposite of the one of `gsub()`. Default is `FALSE` which means that strings that are exactly matched will be replaced. This is useful because strings like `THETA(1)` contains special characters. Use `fixed=FALSE` to use regular expressions. Also, see other arguments accepted by `gsub()` for advanced features.
##' @return A function that runs `gsub` to character vectors
##' @examples
##' myfun <- overwrite("b","d")
##' myfun(c("a","b","c","abc"))
##' ## regular expressions
##' myfun2 <- overwrite("b.*","d",fixed=FALSE)
##' myfun2(c("a","b","c","abc"))
##' @export
overwrite <- function(...,fixed=TRUE){
    
    function(x){
        do.call(gsub,
                append(list(...),list(x=x,fixed=fixed))
                )
    }
}
