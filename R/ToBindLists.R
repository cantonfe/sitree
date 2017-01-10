
########################
## DEFINE CLASS TREELIST
########################
toBindLists <- function(x,y){
  if (is.vector(x)) {
    invisible(c(x,y))
  } else {
    if (is.data.frame(x)) {
      invisible(rbind(x,y))
    } else{
      if (is.factor(x)) {
        ## check that there is no conflict with levels
        if (!is.factor (y))  y <- as.factor(y)
        if (!all(levels(y) %in% levels(x))) {
          levels(x) <- unique(c(levels(x), levels(y)))
        }
        x[(length(x) +1 ): (length(x)+length(y))] <- y
        invisible(x)
      } else stop("not a vector, or a data frame or a factor")
    } 
  }
}
