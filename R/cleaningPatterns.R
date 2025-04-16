cleaningPatterns <- function(clean){
    if(! clean %in% 1:4){
        stop ("only clean values 1, 2, 3, and 4 are supported")
    }
    c("nonmem","worker*","FDATA*","fort.*","WK_*","temp_dir","FSUBS*")
}
