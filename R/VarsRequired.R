
fn.vars.required <- function(my.functions, ...){
####################
  ## READ VARIABLES REQUIRED
####################
  
  extract.formals <- function(x) {
    vv <- names(formals(x))
    vv <- vv[!vv %in% c('tr', 'fl', '...')]
    return(vv)
  }
  is.arg.function <- function(x)  {
    x[sapply(x, function(x) if (exists(x)) is.function(get(x)) else FALSE)]
  }
  vars.required <- numeric(0)

  ## ellipsis
  fn <- list(...) 
  
  fn.chr <- lapply(fn, function(x) ifelse (is.character(x), x, NA))
  fn.chr <- unlist(unname(fn.chr))
  fn.chr <- fn.chr[!is.na(fn.chr)]
  vars.required <- unique(c(vars.required, fn.chr))

  ## if it is a data.frame
  fn <- lapply(fn, function(x) ifelse (is.data.frame(x), NA, x))
  fn <- lapply(fn, function(x) ifelse (is.function(x), NA, x))
  fn <- fn[!is.na(fn)]
  
  fn <- unlist(unname(fn))
  vars.required <- unique(c(vars.required, fn))
  fn <- fn[unlist(lapply(fn, function(x) !is.null(x)))] ## remove nulls
  fn <- is.arg.function(fn)
  
  for(my.fn2 in fn){
    if (is.function (get(my.fn2))){
      fn2 <- extract.formals(get(my.fn2[[1]]))
      vars.required <- unique(c(vars.required, fn2))
    }
  }
  ## Main functions
  for (my.fn in unlist(unname(my.functions))){
    ##print(my.fn)
    fn <- extract.formals(get(my.fn))
    fn <- fn[unlist(lapply(fn, function(x) !is.null(x)))] ## remove nulls
    fn <- is.arg.function(fn)
    vars.required <- unique(c(vars.required, fn))
    
    if (length(fn) != 0){
      for ( my.fn2 in fn){
        my.fn2 <- extract.formals(get(my.fn2))
        my.fn2 <- my.fn2[unlist(lapply(my.fn2, function(x) !is.null(x)))] 
        my.fn2 <- is.arg.function(my.fn2)
        vars.required <- unique(c(vars.required, my.fn2))
      }
    }
  }
  return(vars.required)

}

## reassignInPackage("fn.vars.required", "sitree", fn.vars.required)
